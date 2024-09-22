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

(defcustom dapdbg-lldb-command-line '("lldb-dap-18")
  "Command-line to invoke the lldb debugger process.

The LLDB debugger ships with a separate binary for the DAP server
- on older versions this is called 'lldb-vscode', and it is
called 'lldb-dap' since version 18."
  :type `(repeat string))

(defcustom dapdbg-gdb-command-line '("rust-gdb" "-i" "dap")
  "Command-line to invoke the gdb debugger process.

The GDB debugger implements the DAP interface with the
command-line flags '-i dap'. Note that GDB supports DAP from
version 14 onwards."
  :type `(repeat string))

(defcustom dapdbg-lldb-init-commands
  '("command script import ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_lookup.py"
    "command source ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_commands")
  "Additional properties to pass to the launch command."
  :type `(repeat string))

(defcustom dapdbg-gdb-init-commands nil
  "Additional properties to pass to the launch command."
  :type `(repeat string))

;; ------------------- initialization and session ---------------------

(cl-defstruct dapdbg-session
  "Represents the state of the current connection to the debugger,
for keeping track of things like initialization state, request-response callbacks,
breakpoints and latest thread/stack state when stopped."
  (process nil :read-only t)
  (type nil :read-only t)
  (callbacks (make-hash-table :test 'equal) :read-only t)
  (source-breakpoints (make-hash-table :test 'equal) :read-only t)
  (buffer "")
  (seq 0)
  (thread-id nil)
  (launch-args nil)
  (config-done nil)
  (capabilities nil))

(defvar dapdbg--ssn (make-dapdbg-session)
  "The global session object (only one session is allowed).")

(defconst dapdbg--lldb-type "lldb-dap")
(defconst dapdbg--gdb-type "gdb-dap")

(defun dapdbg--connect (command-line debugger-type)
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
    (setq dapdbg--ssn (make-dapdbg-session :process proc :type debugger-type))
    (when prev-ssn
      (let ((old-bps (dapdbg-session-source-breakpoints prev-ssn))
            (new-bps (dapdbg-session-source-breakpoints dapdbg--ssn)))
        (maphash (lambda (k v) (puthash k v new-bps))
                 old-bps)))
    ))

(defun dapdbg--make-launch-request-args (debugger-type program &optional program-arguments additional-args)
  "Send a launch request to the debugger. ADDITIONAL-ARGS is a plist
of extra arguments to the launch request (which is
debugger-dependent)."
  (let ((launch-args (list
                      :name (file-name-nondirectory program)
                      :type debugger-type
                      :request "launch"
                      :program (expand-file-name program))))
    (when program-arguments
      (plist-put launch-args :args program-arguments))
    (setq launch-args (nconc launch-args additional-args))
    launch-args))

(defun dapdbg--request-initialize (&optional callback)
  "Send an initialize request - this is stage 1 of the
initialization process. See
https://microsoft.github.io/debug-adapter-protocol/overview#initialization."
  (let ((args (list
               :clientID "emacs"
               :clientName "Emacs dapdbg"
               :adapterID "dapdbg"
               :pathFormat "path"
               :linesStartAt1 t
               :columnsStartAt1 t
               :supportsVariableType t
               :supportsVariablePaging t
               :supportsRunInTerminalRequest nil
               :supportsMemoryReferences t
               :locale "en-us")))
    (dapdbg--send-request "initialize" args
                          (lambda (parsed-msg)
                            (dapdbg--handle-initialize-response parsed-msg)
                            (when callback
                              (funcall callback parsed-msg))))))

(defun dapdbg--handle-initialize-response (msg)
  "Handle the response to the initialize request, which contains the
capabilities of the debugger."
  (let ((caps (gethash "body" msg)))
    (dolist (cap '("supportsConditionalBreakpoints" "supportsConfigurationDoneRequest"))
      (unless (gethash cap caps)
        (dapdbg-disconnect)
        (error "Debugger does not have capability \"%s\"" cap)))
    (setf (dapdbg-session-capabilities dapdbg--ssn) caps)))

(defun dapdbg--start-lldb (command-line)
  "Start or restart the LLDB debugger, and launch the program from COMMAND-LINE."
  ;; LLDB only completes initialization after the target is loaded, so needs a fresh session for each launch
  (when (dapdbg--connected-p)
    (pcase (substring (downcase (read-string "A debugger is already running, terminate and replace it (y/n)? " "y")) 0 1)
      ("y" (dapdbg-quit))
      (_ (error "Aborted"))))
  (dapdbg--connect dapdbg-lldb-command-line dapdbg--lldb-type)
  (let* ((toks (string-split command-line nil t))
         (program (car toks))
         (program-arguments (cdr toks)))
    (unless (> (length toks) 0)
      (error "empty command-line"))
    (dapdbg--request-initialize
     (lambda (response)
       ;; https://github.com/llvm/llvm-project/tree/012dbec604c99a8f144c4d19357e61b65d2a7b78/lldb/tools/lldb-dap#launching--attaching-configuration
       (let ((extra-launch-args
              (list :initCommands dapdbg-lldb-init-commands
                    :customFrameFormat "${module.file.basename} ${function.name-without-args}"
                    :stopOnEntry t)))
         (dapdbg--send-request "launch"
                               (dapdbg--make-launch-request-args dapdbg--lldb-type program program-arguments extra-launch-args)))))))

(defun dapdbg--start-gdb (command-line)
  "Use GDB to launch and debug the program and arguments given in
COMMAND-LINE. If a GDB process
is already running in the current session, it will be re-used."
  ;; GDB initializes before the target is loaded, so sessions can be re-used.
  (let ((connected nil))
    (if (dapdbg--connected-p)
        (if (equal (dapdbg-session-type dapdbg--ssn) dapdbg--gdb-type)
            (setq connected t)
          (pcase (substring (downcase (read-string "Another debugger is running, terminate and replace it (y/n)? " "y")) 0 1)
            ("y" (dapdbg-quit))
            (_ (error "Aborted")))))
    ;; https://sourceware.org/gdb/current/onlinedocs/gdb.html/Debugger-Adapter-Protocol.html
    (let* ((toks (string-split command-line nil t))
           (program (car toks))
           (program-arguments (cdr toks))
           (extra-launch-args (list :stopAtBeginningOfMainSubprogram t))
           (launch-args (dapdbg--make-launch-request-args dapdbg--gdb-type program program-arguments extra-launch-args)))
      (unless (> (length toks) 0)
        (error "empty command-line"))
      (if connected
          (dapdbg--send-request "launch"
                                (dapdbg--make-launch-request-args dapdbg--lldb-type program program-arguments extra-launch-args))
        (dapdbg--connect dapdbg-gdb-command-line dapdbg--gdb-type)
        ;; set the launch to occur after the configurationDone event:
        (setf (dapdbg-session-launch-args dapdbg--ssn) launch-args)
        (dapdbg--request-initialize)))))

(defun dapdbg--connected-p ()
  (and dapdbg--ssn
       (process-live-p (dapdbg-session-process dapdbg--ssn))))

(defun dapdbg--do-initial-config ()
  "Chain a set of requests to set breakpoint configuration and so
on, after which make a configurationDone request."
  (let ((bp-table (dapdbg-session-source-breakpoints dapdbg--ssn))
        (chain-args nil))
    (maphash (lambda (filename source-table)
               (let* ((bp-args (dapdbg--make-set-breakpoints-args filename source-table))
                      (chain-elt (list :command "setBreakpoints" :args bp-args
                                       :callback (apply-partially #'dapdbg--update-breakpoint-table filename))))
                 (push chain-elt chain-args)))
             bp-table)
    ;; TODO: set function breakpoints
    ;; TODO: set exception breakpoints
    (push (list :command "configurationDone" :args nil
                :callback #'dapdbg--handle-response-configdone)
          chain-args)
    (dapdbg--chain-requests (nreverse chain-args))))

(defun dapdbg--handle-response-configdone (msg)
  "When the debugger has finished configuration, if there are launch args pending, send the launch request."
  (setf (dapdbg-session-config-done dapdbg--ssn) t)
  (let ((args (dapdbg-session-launch-args dapdbg--ssn)))
    (when args
      (dapdbg--send-request "launch" args))))

(defun dapdbg--handle-event-initialized (parsed-msg)
  "Once the intialized event is recieved - set exception behaviour and any breakpoints."
  (dapdbg--do-initial-config))

(defun dapdbg-quit ()
  "Shut down and disconnect from the currently running DAP server (if any)."
  (interactive)
  (if dapdbg--ssn
      (let ((proc (dapdbg-session-process dapdbg--ssn)))
        (if (process-live-p proc)
            (kill-process proc)))))

;; ------------------- commands ---------------------

(defmacro dapdbg--make-thread-command (name command docstring &optional args capability)
  `(defun ,(intern (format "dapdbg-%s" name)) ()
     ,docstring
     (interactive)
     ,(if capability
          `(unless (gethash ,capability (dapdbg-session-capabilities dapdbg--ssn))
             (error "Debugger does not have capability: \"%s\"" ,capability)))
     (let ((pargs (list :threadId (dapdbg-session-thread-id dapdbg--ssn))))
       ,(when args `(nconc pargs ,args))
       (dapdbg--send-request ,command pargs))))

(dapdbg--make-thread-command "next" "next" "Step one line (skip functions).")
(dapdbg--make-thread-command "nexti" "next" "Step one instruction (skip functions)." '(:granularity "instruction") "supportsSteppingGranularity")
(dapdbg--make-thread-command "step" "stepIn" "Step one source line.")
(dapdbg--make-thread-command "stepi" "stepIn" "Step one instruction." '(:granularity "instruction") "supportsSteppingGranularity")
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
    (if (dapdbg--connected-p)
        (dapdbg--request-set-breakpoints-for-file filename)
      (let ((source-table (dapdbg--get-or-create-source-table filename))
            (updated-table (make-hash-table :test 'equal)))
        (puthash filename source-table updated-table)
        (run-hook-with-args 'dapdbg--breakpoints-updated-callback-list updated-table)))))

(defun dapdbg--make-set-breakpoints-args (filename source-table)
  "Return the arguments for a setBreakpoints request for the source FILENAME."
  (list :source (list :name (file-name-nondirectory filename) :path filename)
        :breakpoints (vconcat (mapcar (lambda (n) (list :line n)) (hash-table-keys source-table)))))

(defun dapdbg--request-set-breakpoints-for-file (filename &optional callback)
  "Request the debugger to update the breakpoints for the source FILENAME."
  (let ((source-table (dapdbg--get-or-create-source-table filename)))
    (dapdbg--send-request
     "setBreakpoints"
     (dapdbg--make-set-breakpoints-args filename source-table)
     (lambda (response)
       (dapdbg--update-breakpoint-table filename response)
       (if callback
           (funcall callback response))))))

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

;; ------------------- disassembly ---------------------

;; The "offset" and "instructionOffset" arguments cannot be trusted (at the time
;; of writing).
;;
;; LLDB ignores the offset argument and treats the instructionOffset as a byte
;; offset. https://github.com/llvm/llvm-project/blob/012dbec604c99a8f144c4d19357e61b65d2a7b78/lldb/tools/lldb-dap/lldb-dap.cpp#L4089.
;;
;; GDB fetches data at a start address accounting for the (byte) offset, but
;; indexes into the resulting array using the instruction offset, which
;; therefore cannot be used as a negative offset from the start
;; address. https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=gdb/python/lib/gdb/dap/disassemble.py;h=a2e27e54a6408487f8c32b358389fd21b3eac179;hb=refs/heads/master#l74
;;
;; Therefore (because instruction size and aligment are very platform-dependent)
;; we cannot really guess any start address for our block before the current
;; instruction pointer. This is a shame as it is generaly nice to be able to see
;; the prevoius instructions, for context.

(defun dapdbg--request-disassembly (program-counter &optional instruction-count callback)
  "Get disassembly for INSTRUCTION-COUNT instructions starting at PROGRAM-COUNTER."
  (unless (gethash "supportsDisassembleRequest" (dapdbg-session-capabilities dapdbg--ssn))
    (error "disassemble request is not supported by the current debugger"))
  (let* ((disassemble-args (list :memoryReference (format "0x%x" program-counter)
                                 :resolveSymbols t
                                 :instructionCount (or instruction-count 128))))
    (dapdbg--send-request "disassemble" disassemble-args callback)))

(defun dapdbg--parse-address (strval)
  "Parse the given string into a number, accounting for hex numbers with leading `0x'."
  (let  ((s-val (if (string= (downcase (substring strval 0 2)) "0x")
                    (substring strval 2)
                  strval)))
    (string-to-number s-val 16)))

;; ------------------- internal requests ---------------------

(defun dapdbg--request-threads (&optional callback)
  (dapdbg--send-request "threads" nil callback))

(defun dapdbg--request-stacktrace (&optional thread-id callback)
  "Request a stacktrace for THREAD-ID (defaults to currnt
thread). CALLBACK will be called with 2 arguments: thread-id and
the `StackTraceResponse' parsed message."
  (let ((tid thread-id))
    (unless tid
      (setq tid (dapdbg-session-thread-id dapdbg--ssn)))
    (dapdbg--send-request "stackTrace" (list :threadId tid) (apply-partially callback tid))))

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
module), return a cons pair of (header . body)
<https://microsoft.github.io/debug-adapter-protocol/overview#base-protocol>"
  (let* ((json-false :json-false)
         (body (json-encode msg-plist)))
    (cons (format "Content-Length: %d\r\n" (string-bytes body)) body)))

(defun dapdbg--create-request (command &optional args)
  "Create a request for COMMAND with optional ARGS supplied as a
tree structure of plists. Returns a plist with properties
`:header' `:body' and `:seq'."
  (let* ((seq (dapdbg--seq-and-inc))
         (request-plist (list
                         :command command
                         :seq seq
                         :type "request")))
    (when args
      (plist-put request-plist :arguments args))
    (let ((hdr-body (dapdbg--base-protocol request-plist seq)))
      (list :header (car hdr-body) :body (cdr hdr-body) :seq seq))))

(defun dapdbg--register-callback (seq callback)
  (puthash seq callback (dapdbg-session-callbacks dapdbg--ssn)))

(defun dapdbg--send-request (command &optional args callback)
  "Send a requests of type COMMAND to the debugger, ARGS is the set
of arguments as a plist."
  (pcase-let ((`(:header ,hdrs :body ,body :seq ,seq) (dapdbg--create-request command args)))
    (dapdbg--log-io-message hdrs body t)
    (if callback
        (dapdbg--register-callback seq callback))
    (process-send-string (dapdbg-session-process dapdbg--ssn) (format "%s\r\n%s" hdrs body))))

(defun dapdbg--chain-requests (requests)
  "Excecute a set of requests in sequence. Each element of REQUESTS
is a plist with properties `:command' (request type),
`:args' (arguments to the request) and `:callback' (function to
call with the result), invoking `dapdbg--send-request' each time."
  (pcase-let ((`(:command ,req-type :args ,req-args :callback ,callback) (car requests)))
    (let* ((remaining (cdr requests))
           (handler (lambda (parsed-msg)
                      (when callback
                        (funcall callback parsed-msg))
                      (when remaining
                        (dapdbg--chain-requests remaining)))))
      (dapdbg--send-request req-type req-args handler))))

(defcustom dapdbg-io-log-flag nil
  "Enables output of request & response messages to/from the DAP
   server (only useful for debugging this package)"
  :type 'boolean)

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
    ("process" (let ((body (gethash "body" parsed-msg)))
                 (message "%sed %s, pid: %d"
                          (gethash "startMethod" body)
                          (gethash "name" body)
                          (gethash "systemProcessId" body))))
    (`,an-event (message "unhandled event: %s" an-event))))

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
      (nreverse chunks))))

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
          (dapdbg--log-io-message hdrs body nil)
          (list
           :parsed-length end
           :parsed-msg (dapdbg--parse-json (substring body 0 content-length))))))))

;; ------------------- internal tools ---------------------

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

(defun dapdbg--get-io-log-buffer ()
  "Buffer to display request/response messages if dapdbg-print-io is t"
  (let ((buf-created (dapdbg--get-or-create-buffer "*dapdbg IO*")))
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (js-json-mode)
        (font-lock-mode -1)))
    (car buf-created)))

(defun dapdbg--log-io-message (hdrs msg is-request)
  (when dapdbg-io-log-flag
    (with-current-buffer (dapdbg--get-io-log-buffer)
      (goto-char (point-max))
      (let ((beg (point)))
        (insert (format "- %s ------------- %s -\n" (if is-request "TX" "RX") (format-time-string "%Y-%m-%d %H:%M:%S")))
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
