;; dapdbg.el --- Debug Adapter Protocol for native debuggers LLDB and GDB -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stewart Perry

;; Author: Stewart Perry <stewart.c.perry@gmail.com>
;; Created: 31 Aug 2024

;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This file is free software

;;; Commentary:

;; This package is a simple implementation of the Debug Adapter Protocol (DAP)
;; intended to be used for direct communication with native debuggers such as
;; GDB (since version 14) and LLDB.
;;
;; The following is a fairly minimal implementation of the DAP for this narrow
;; use-case; it runs the debugger as a sub-process and communicates with it in
;; single request/response mode.
;;
;; This implementation specifically supports native-debugging features such as
;; instruction-level stepping and disassembly/memory views. It does not support
;; multiple sessions, socket connections and so forth; for a generalized DAP
;; implementation supporting multiple scripting and native languages, see
;; [dap-mode](https://emacs-lsp.github.io/dap-mode/).
;;
;; For the full specification of DAP, see
;; https://microsoft.github.io/debug-adapter-protocol/specification.

;;; Code:

(require 'json)
(require 'cl-lib)

;; ------------------- custom variables ---------------------

(defvar dapdbg-lldb-command-line '("lldb-dap-18")
  "Command-line to invoke the lldb debugger process.

The LLDB debugger ships with a separate binary for the DAP server
- on older versions this is called 'lldb-vscode', and it is
called 'lldb-dap' since version 18.")

(defvar dapdbg-gdb-command-line '("gdb" "-i" "dap")
  "Command-line to invoke the gdb debugger process.

The GDB debugger implements the DAP interface with the
command-line flags '-i dap'. Note that GDB supports DAP from
version 14 onwards.")

(defvar dapdbg-lldb-init-commands
  '("command script import ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_lookup.py"
    "command source ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_commands")
  "Additional properties to pass to the launch command.")

(defvar dapdbg-gdb-init-commands nil
  "Additional properties to pass to the launch command.")

;; ------------------- initialization and session ---------------------

(cl-defstruct dapdbg-session
  "Represents the state of the current connection to the debugger,
for keeping track of things like initialization state, request-response callbacks,
breakpoints and latest thread/stack state when stopped."
  (process nil :read-only t)
  (callbacks (make-hash-table :test 'equal) :read-only t)
  (source-breakpoints (make-hash-table :test 'equal) :read-only t)
  (prompt nil)
  (sends-initialized-event t)
  (buffer "")
  (seq 0)
  (thread-id nil)
  (launch-args nil)
  (config-done nil)
  (capabilities nil))

(defvar dapdbg--ssn (make-dapdbg-session)
  "The global session object (only one session is allowed).")

(defconst dapdbg--lldb-plist
  (list :type "lldb-dap"
        :command-line-sym 'dapdbg-lldb-command-line
        :init-commands-sym 'dapdbg-lldb-init-commands
        :sends-initialized-event nil
        :prompt "(lldb)"
        :stop-on-entry-sym :stopOnEntry)
  "Configuration specific to the LLDB debugger.")

(defconst dapdbg--gdb-plist
  (list :type "gdb-dap"
        :command-line-sym 'dapdbg-gdb-command-line
        :init-commands-sym 'dapdbg-gdb-init-commands
        :sends-initialized-event t
        :prompt "(gdb)"
        :stop-on-entry-sym :stopAtBeginningOfMainSubprogram)
  "Configuration specific to the GDB debugger.")

(defun dapdbg--connect (command-line)
  "Start up the debugger in a sub-process, ready for communication."
  (let ((proc (make-process
               :name "dapdbg-session"
               :connection-type 'pipe
               :coding 'no-conversion
               :command command-line
               :stderr "*debugger stderr*"
               :filter #'dapdbg--handle-server-message
               :noquery t))
        (prev-ssn dapdbg--ssn))
    (setq dapdbg--ssn (make-dapdbg-session :process proc))
    (when prev-ssn
      (let ((old-bps (dapdbg-session-source-breakpoints prev-ssn))
            (new-bps (dapdbg-session-source-breakpoints dapdbg--ssn)))
        (maphash (lambda (k v) (puthash k v new-bps))
                 old-bps)))
    ))

(defun dapdbg--request-initialize (program program-arguments debugger-plist)
  "Send an initialize request - this is stage 1 of the
initialization process. See
https://microsoft.github.io/debug-adapter-protocol/overview#initialization."
  (let ((arguments (list
                    :name (file-name-nondirectory program)
                    :type (plist-get debugger-plist :type)
                    :request "launch"
                    :program (expand-file-name program)
                    (plist-get debugger-plist :stop-on-entry-sym) t))
        (init-commands (symbol-value (plist-get debugger-plist :init-commands-sym))))
    (when program-arguments
      (plist-put arguments :args program-arguments))
    (when init-commands
      (plist-put arguments :initCommands init-commands))
    (setf (dapdbg-session-launch-args dapdbg--ssn) arguments))
  (let ((args (list
               :clientID "dapdbg"
               :clientName "dapdbg"
               :adapterID "dapdbg"
               :pathFormat "path"
               :linesStartAt1 t
               :columnsStartAt1 t
               :supportsVariableType t
               :supportsVariablePaging t
               :supportsRunInTerminalRequest nil
               :supportsMemoryReferences t
               :locale "en-us")))
    (dapdbg--send-request "initialize" args #'dapdbg--handle-initialize-response)))

(defun dapdbg--connect-and-initialize (command-line debugger-plist)
  "Common start method for all debugger types."
  (let ((toks (string-split command-line nil t)))
    (unless (> (length toks) 0)
      (error "empty command-line"))
    (when (and dapdbg--ssn (process-live-p (dapdbg-session-process dapdbg--ssn)))
      (pcase (substring (downcase (read-string "A debugger is already running, terminate and replace it (y/n)? " "y")) 0 1)
        ("y" (dapdbg-quit))
        (_ (error "Aborted"))))
    (dapdbg--connect (symbol-value (plist-get debugger-plist :command-line-sym)))
    (setf (dapdbg-session-prompt dapdbg--ssn) (plist-get debugger-plist :prompt))
    (setf (dapdbg-session-sends-initialized-event dapdbg--ssn) (plist-get debugger-plist :sends-initialized-event))
    (dapdbg--request-initialize (car toks) (cdr toks) debugger-plist)))

(defun dapdbg--start-lldb (command-line)
  "Start up the LLDB debugger."
  (dapdbg--connect-and-initialize command-line dapdbg--lldb-plist))

(defun dapdbg--start-gdb (command-line)
  "Start up the GDB debugger."
  (dapdbg--connect-and-initialize command-line dapdbg--gdb-plist))

(defun dapdbg--ready-p ()
  (and dapdbg--ssn
       (process-live-p (dapdbg-session-process dapdbg--ssn))
       (dapdbg-session-config-done dapdbg--ssn)))

(defun dapdbg--handle-initialize-response (msg)
  (let ((caps (gethash "body" msg)))
    (dolist (cap '("supportsConditionalBreakpoints" "supportsConfigurationDoneRequest"))
      (unless (gethash cap caps)
        (dapdbg-disconnect)
        (error "Debugger does not have capability \"%s\"" cap)))
    (setf (dapdbg-session-capabilities dapdbg--ssn) caps))
  (unless (dapdbg-session-sends-initialized-event dapdbg--ssn)
    ;; For GDB we just wait for the initialized event, but LLDB seems to require
    ;; things in an order which doesn't match the spec
    (dapdbg--set-all-breakpoints #'dapdbg--request-launch-then-configuration-done)))

(defun dapdbg--request-launch-then-configuration-done ()
  (let ((args (dapdbg-session-launch-args dapdbg--ssn)))
    (dapdbg--send-request "launch" args
                          (lambda (_msg)
                            (dapdbg--send-request "configurationDone" nil #'dapdbg--handle-response-configdone)))))

(defun dapdbg--request-launch ()
  (let ((args (dapdbg-session-launch-args dapdbg--ssn)))
    (dapdbg--send-request "launch" args nil)))

(defun dapdbg--handle-response-configdone (msg)
  (setf (dapdbg-session-config-done dapdbg--ssn) t)
  (when (dapdbg-session-sends-initialized-event dapdbg--ssn)
    (dapdbg--request-launch)))

(defun dapdbg--request-configuration-done ()
  "At then end of the initialization sequence, send \"configurationDone\" and do launch afterwards."
  (dapdbg--send-request "configurationDone" nil #'dapdbg--handle-response-configdone))

(defun dapdbg--handle-event-initialized (parsed-msg)
  "Once the intialized event is recieved - set any breakpoints, then
proceed with the initialization sequence."
  (when (dapdbg-session-sends-initialized-event dapdbg--ssn)
    (dapdbg--set-all-breakpoints #'dapdbg--request-configuration-done)))

(defun dapdbg--initialized-p ()
  "Check that the debugger is alive and has initialized. See
https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Initialize."
  (and dapdbg--ssn
       (process-live-p (dapdbg-session-process dapdbg--ssn))
       (dapdbg-session-capabilities dapdbg--ssn)))

(defun dapdbg-quit ()
  "Shut down and disconnect from the currently running DAP server (if any)."
  (interactive)
  (if dapdbg--ssn
      (let ((proc (dapdbg-session-process dapdbg--ssn)))
        (if (process-live-p proc)
            (kill-process proc)))))

;; ------------------- commands ---------------------

(defmacro dapdbg--make-thread-command (name command docstring &optional args)
  `(defun ,(intern (format "dapdbg-%s" name)) ()
     ,docstring
     (interactive)
     (let ((pargs (list :threadId (dapdbg-session-thread-id dapdbg--ssn))))
       ,(when args `(nconc pargs ,args))
       (dapdbg--send-request ,command pargs))))

(dapdbg--make-thread-command "next" "next" "Step one line (skip functions).")
(dapdbg--make-thread-command "nexti" "next" "Step one instruction (skip functions)." '(:granularity "instruction"))
(dapdbg--make-thread-command "step" "stepIn" "Step one source line.")
(dapdbg--make-thread-command "stepi" "stepIn" "Step one instruction." '(:granularity "instruction"))
(dapdbg--make-thread-command "finish" "stepOut" "Finish executing the current function.")
(dapdbg--make-thread-command "continue" "continue" "Resume exceution.")

;; ------------------- breakpoint logic ---------------------

(defun dapdbg--get-or-create-source-table (filename)
  "Get the breakpoint hashtable for the given filename, which is
keyed by line number. Create one if it does not already exist."
  (let* ((bp-table (dapdbg-session-source-breakpoints dapdbg--ssn))
         (source-table (gethash filename bp-table)))
    (unless source-table
      (setq source-table (make-hash-table :test 'eql))
      (puthash filename source-table bp-table))
    source-table))

(defun dapdbg-toggle-breakpoint (&optional filename linenumber)
  "Toggle the breakpoint request for the given filename and line
number. If the debugger is running, this will create a request to
update the breakpoints for that source file.

When called interactively, take the location from point in the
current buffer."
  (interactive)
  (unless filename
    (setq filename (expand-file-name (buffer-file-name))))
  (unless linenumber
    (setq linenumber (line-number-at-pos)))
  (let ((source-table (dapdbg--get-or-create-source-table filename)))
    (let ((existing-bp (gethash linenumber source-table)))
      (if existing-bp
          (remhash linenumber source-table)
        (puthash linenumber linenumber source-table)))
    (if (dapdbg--ready-p)
        (dapdbg--request-set-breakpoints-for-file filename)
      (let ((source-table (dapdbg--get-or-create-source-table filename))
            (updated-table (make-hash-table :test 'equal)))
        (puthash filename source-table updated-table)
        (run-hook-with-args 'dapdbg--breakpoints-updated-callback-list updated-table)))))

(defun dapdbg--request-set-breakpoints-for-file (filename &optional callback)
  "Request the debugger to update the breakpoints for the source FILENAME."
  (let ((source-table (dapdbg--get-or-create-source-table filename)))
    (dapdbg--send-request
     "setBreakpoints"
     (list :source (list :name (file-name-nondirectory filename) :path filename)
           :breakpoints (vconcat (mapcar (lambda (n) (list :line n)) (hash-table-keys source-table))))
     (lambda (parsed-msg)
       (dapdbg--update-breakpoint-table filename parsed-msg)
       (if callback
           (funcall callback parsed-msg))))))

(defun dapdbg--set-breakpoint-in-chain (remaining-filenames final-callback _parsed-msg)
  (if remaining-filenames
      (dapdbg--request-set-breakpoints-for-file
       (car remaining-filenames)
       (apply-partially #'dapdbg--set-breakpoint-in-chain (cdr remaining-filenames) final-callback))
    (when final-callback ; no filenames remaining
      (funcall final-callback))))

(defun dapdbg--set-all-breakpoints (&optional final-callback)
  "Chain a set of requests to set source breakpoints for all
filenames, then call any final callback."
  (let* ((bp-table (dapdbg-session-source-breakpoints dapdbg--ssn))
         (filenames (hash-table-keys bp-table)))
    (dapdbg--set-breakpoint-in-chain filenames final-callback nil)))

(defun dapdbg--update-breakpoint-table (filename bp-response)
  (unless (equal "setBreakpoints" (gethash "command" bp-response))
    (error "not a breakpoint response"))
  (let ((source-table (dapdbg--get-or-create-source-table filename))
        (updated-table (make-hash-table :test 'equal)))
    (puthash filename source-table updated-table)
    (dolist (bp-details (gethash "breakpoints" (gethash "body" bp-response)))
      (when (gethash "verified" bp-details)
        (puthash (gethash "line" bp-details) bp-details source-table)))
    (run-hook-with-args 'dapdbg--breakpoints-updated-callback-list updated-table)))

(defun dapdbg--update-breakpoint-table-single-bp (bp-details &optional remove)
  (let* ((filename (gethash "path" (gethash "source" bp-details)))
         (linenumber (gethash "line" bp-details))
         (source-table (dapdbg--get-or-create-source-table filename))
         (updated-table (make-hash-table :test 'equal)))
    (puthash filename source-table updated-table)
    (if remove
        (remhash linenumber  source-table)
      (puthash linenumber bp-details source-table))
    (run-hook-with-args 'dapdbg--breakpoints-updated-callback-list updated-table)))

;; ------------------- internal requests ---------------------

(defun dapdbg--request-stacktrace (&optional thread-id callback)
  (let ((tid thread-id))
    (unless tid
      (setq tid (dapdbg-session-thread-id dapdbg--ssn)))
    (dapdbg--send-request "stackTrace" (list :threadId tid) callback)))

(defun dapdbg--request-scopes (frame-id callback)
  (dapdbg--send-request "scopes" (list :frameId frame-id) callback))

(defun dapdbg--request-variables (ref-id callback)
  (dapdbg--send-request "variables" (list :variablesReference ref-id) callback))

(defun dapdbg--request-memory-dump (start-address count &optional callback)
  (dapdbg--send-request "readMemory" (list :memoryReference start-address :count count) callback))

(defun dapdbg--request-eval-repl (expr &optional callback)
  (dapdbg--send-request "evaluate" (list :expression expr :context "repl") callback))

;; ------------------- event handlers ---------------------

(defvar dapdbg--breakpoints-updated-callback-list nil
  "Functions to call when the this session's breakpoints have been
updated. The callback receives a hash table of filename ->
breakpoint table mappings.")

(defvar dapdbg--stopped-callback-list nil
  "Functions to call when the debugger sends a 'stopped' event. The
   callbacks receive the event message (i.e. this is an abnormal
   hook).")

(defvar dapdbg--continued-callback-list nil
  "Functions to call when the debugger sends a 'continued' event.")

(defvar dapdbg--output-callback-list nil
  "Functions to call when the debugger sends an 'output' event.")

(defvar dapdbg--thread-callback-list nil
  "Functions to call when the debugger sends an 'thread' event.")

(defvar dapdbg--breakpoint-callback-list nil
  "Functions to call when the debugger sends an 'breakpoint' event.")

(defun dapdbg--handle-event-stopped (msg)
  (let ((tid (gethash "threadId" (gethash "body" msg))))
    (setf (dapdbg-session-thread-id dapdbg--ssn) tid)))

(defun dapdbg--handle-event-breakpoint (msg)
  (let* ((body (gethash "body" msg))
         (bp-details (gethash "breakpoint" body)))
    (pcase (gethash "reason" body)
      ((or "new" "changed")
       (dapdbg--update-breakpoint-table-single-bp bp-details))
      ("removed"
       (dapdbg--update-breakpoint-table-single-bp bp-details t)))))

;; ------------------- base protocol ---------------------

(defun dapdbg--seq-and-inc ()
  (let ((seq-value (dapdbg-session-seq dapdbg--ssn)))
    (setf (dapdbg-session-seq dapdbg--ssn) (1+ seq-value))
    seq-value))

(defun dapdbg--base-protocol (msg-plist seq)
  "Translate MSG-PLIST (a tree structure expected by the json
module), return a string which is the header+body format required
by the DAP protocol
<https://microsoft.github.io/debug-adapter-protocol/overview#base-protocol>"
  (let* ((json-false :json-false)
         (body (json-encode msg-plist)))
    (list
     :header (format "Content-Length: %d\r\n" (string-bytes body))
     :body body
     :seq seq)))

(defun dapdbg--create-request (command &optional args)
  (let* ((seq (dapdbg--seq-and-inc))
         (request-plist (list
                         :command command
                         :seq seq
                         :type "request")))
    (when args
      (plist-put request-plist :arguments args))
    (dapdbg--base-protocol request-plist seq)))

(defun dapdbg--register-callback (seq callback)
  (puthash seq callback (dapdbg-session-callbacks dapdbg--ssn)))

(defun dapdbg--send-request (command &optional args callback)
  (pcase-let ((`(:header ,hdrs :body ,body :seq ,seq) (dapdbg--create-request command args)))
    (dapdbg--io-message hdrs body t)
    (if callback
        (dapdbg--register-callback seq callback))
    (process-send-string (dapdbg-session-process dapdbg--ssn) (format "%s\r\n%s" hdrs body))))

(defun dapdbg--handle-event (parsed-msg)
  (pcase (gethash "event" parsed-msg)
    ("initialized"
     (dapdbg--handle-event-initialized parsed-msg))
    ("stopped"
     (dapdbg--handle-event-stopped parsed-msg)
     (run-hook-with-args 'dapdbg--stopped-callback-list parsed-msg))
    ("breakpoint"
     (dapdbg--handle-event-breakpoint parsed-msg)
     (run-hook-with-args 'dapdbg--breakpoint-callback-list parsed-msg))
    ("continued" (run-hook-with-args 'dapdbg--continued-callback-list parsed-msg))
    ("output" (run-hook-with-args 'dapdbg--output-callback-list parsed-msg))
    ("thread" (run-hook-with-args 'dapdbg--thread-callback-list parsed-msg))
    (`,an-event (message "event: %s" an-event))))

(defun dapdbg--handle-response (parsed-msg)
  (if (not (gethash "success" parsed-msg))
      (message "[%s] failed - %s" (gethash "command" parsed-msg) (gethash "message" parsed-msg))
    (let* ((cb-seq (gethash "request_seq" parsed-msg))
           (cb-table (dapdbg-session-callbacks dapdbg--ssn))
           (cb (gethash cb-seq cb-table)))
      (when cb
        (remhash cb-seq cb-table)
        (funcall cb parsed-msg)))))

(defun dapdbg--handle-server-message (_process msg)
  (catch 'incomplete-msg
    (let ((remaining (concat (dapdbg-session-buffer dapdbg--ssn) msg))
          (chunks (list)))
      (while (not (string-empty-p remaining))
        (pcase-let ((`(:parsed-length ,length :parsed-msg ,parsed-msg) (dapdbg--parse-message remaining)))
          (if parsed-msg
              (progn
                (pcase (gethash "type" parsed-msg)
                  ("event" (dapdbg--handle-event parsed-msg))
                  ("response" (dapdbg--handle-response parsed-msg))
                  (`,something (message "unrecognized message type %s" something)))
                (add-to-list 'chunks length)
                (setq remaining (substring remaining length)))
            ;; message is incomplete, push it back to the buffer and return
            (setf (dapdbg-session-buffer dapdbg--ssn) remaining)
            (throw 'incomplete-msg nil))))
      (setf (dapdbg-session-buffer dapdbg--ssn) "")
      (reverse chunks))))

(defun dapdbg--parse-json (str)
  (let* ((json-array-type 'list)
         (json-object-type 'hash-table)
         (json-false nil))
    (json-read-from-string str)))

(defun dapdbg--parse-header-line (line)
  (let ((idx (string-search ":" line)))
    (unless idx
      (error "malformed header line: \"%s\"" line))
    (cons (substring line 0 idx) (substring line (+ idx 2)))))

(defun dapdbg--parse-header (hdr)
  (let ((hdr-pairs (mapcar #'dapdbg--parse-header-line (cl-remove-if #'seq-empty-p (string-split hdr "\r\n")))))
    (string-to-number (alist-get "Content-Length" hdr-pairs nil nil #'string=))))

(defconst dapdbg--not-parsed-response
  (list :parsed-length 0 :parsed-msg nil))

(defun dapdbg--parse-message (msg)
  "Attempt to parse a message send from the debugger. The message
may be incomplete - if so it is pushed to a buffer and stiched
onto the start of the next message."
  (catch 'incomplete
    (let ((end-of-header (string-search "\r\n\r\n" msg)))
      (unless end-of-header
        (throw 'incomplete 'dapdbg--not-parsed-response))
      (let* ((hdrs (substring msg 0 (+ end-of-header 2)))
             (content-length (dapdbg--parse-header (substring msg 0 end-of-header)))
             (beg (+ end-of-header 4))
             (end (+ beg content-length)))
        (unless (>= (length msg) (1- end))
          (throw 'incomplete 'dapdbg--not-parsed-response))
        (let ((body (substring msg beg end)))
          (dapdbg--io-message hdrs body nil)
          (list
           :parsed-length end
           :parsed-msg (dapdbg--parse-json (substring body 0 content-length))))))))

;; ------------------- internal tools ---------------------

(defcustom dapdbg-io-print-flag nil
  "Enables output of request & response messages to/from the DAP
   server (only useful for debugging this package)"
  :type 'boolean)

(defface dapdbg-request-face
  '((t :inherit (warning)))
  "Face for requests to the DAP server")

(defface dapdbg-response-face
  '((t :inherit (success)))
  "Face for responses from the DAP server")

(defun dapdbg--get-or-create-buffer (buf-name)
  "Like get-buffer-create, but return a cons cell of the buffer and a flag to say if it was just created"
  (let* ((buf (get-buffer buf-name)))
    (if buf
        (cons buf nil)
      (cons (get-buffer-create buf-name) t))))

(defun dapdbg--io-buf ()
  "Buffer to display request/response messages if dapdbg-print-io is t"
  (let ((buf-created (dapdbg--get-or-create-buffer "*dapdbg IO*")))
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (js-json-mode)
        (font-lock-mode -1)))
    (car buf-created)))

(defun dapdbg--io-message (hdrs msg is-request)
  (when dapdbg-io-print-flag
    (with-current-buffer (dapdbg--io-buf)
      (goto-char (point-max))
      (let ((beg (point)))
        (insert (format "- %s -----------------------------------\n" (if is-request "TX" "RX")))
        (insert (format "%s\r\n" hdrs))
        (add-face-text-property beg (point)
                                (if is-request 'dapdbg-request-face 'dapdbg-response-face)))
      (let ((beg (point)))
        (insert msg)
        (json-pretty-print beg (point))
        (font-lock-fontify-region beg (point)))
      (goto-char (point-max))
      (let ((beg (point)))
        (insert "\n----------------------------------------\n\n")
        (add-face-text-property beg (point)
                                (if is-request 'dapdbg-request-face 'dapdbg-response-face)))
      (goto-char (point-max))
      )))

(defun dapdbg--request-disassembly (callback mem-ref &optional instructions-preceeding instruction-count)
  "Get disassembly for the given session, for the optional
   address range (defaults to 64 instructions either side of the
   instruction pointer)."
  (unless (gethash "supportsDisassembleRequest" (dapdbg-session-capabilities dapdbg--ssn))
    (error "disassemble request is not supported by this debug adapter"))
  (let ((disassemble-args (list :memoryReference (format "0x%x" mem-ref)
                                :instructionOffset (or instructions-preceeding -63)
                                :instructionCount (or instruction-count 64))))
    (dapdbg--send-request "disassemble" disassemble-args
                          (lambda (msg) (funcall callback (gethash "instructions" (gethash "body" msg)))))))

(defun dapdbg--parse-address (strval)
  (let  ((s-val (if (string= (downcase (substring strval 0 2)) "0x")
                    (substring strval 2)
                  strval)))
    (string-to-number s-val 16)))

(defun dapdbg---print-capabilities ()
  "List the capabilities of the current debugger.

   See
   https://microsoft.github.io/debug-adapter-protocol/specification#Types_Capabilities"
  (interactive)
  (message "Capabilities:")
  (let* ((ht (dapdbg-session-capabilities dapdbg--ssn))
         (keys (sort (hash-table-keys ht) 'string<)))
    (dolist (key keys)
      (message "%s: %s" key (gethash key ht)))))

(provide 'dapdbg)

;;; dabdbg.el ends here
