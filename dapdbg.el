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
;; The following is a fairly minimal implementation of the DAP for this
;; specialized use-case; it runs the debugger as a sub-process and communicates
;; with it in single request/response mode. It does not support multiple
;; sessions, socket connections and so forth; for a generalized DAP
;; implementation supporting multiple scripting and native languages, see
;; [dap-mode](https://emacs-lsp.github.io/dap-mode/).
;;
;; For the full specification of DAP, see
;; https://microsoft.github.io/debug-adapter-protocol/specification.

;;; Code:

(require 'json)

(defvar dapdbg-lldb-command-line '("lldb-dap")
  "Command-line to invoke the lldb debugger process.

The LLDB debugger ships with a separate binary for the DAP server
- on older versions this is called 'lldb-vscode', and it is
called 'lldb-dap' since version 18.")

(defvar dapdbg-gdb-command-line '("gdb" "-i" "dap")
  "Command-line to invoke the gdb debugger process.

The GDB debugger implements the DAP interface with the
command-line flags '-i dap'")

(defvar dapdbg-launch-args nil 
  "Additional properties to pass to the launch command (debugger dependent).")

(defface dapdbg-request-face
  '((((class color)) :foreground "Green")
    (t :weight bold))
  "Face for requests to the DAP server")

(defface dapdbg-response-face
  '((((class color)) :foreground "Cyan")
    (t :slant italic))
  "Face for responses from the DAP server")

(defvar dapdbg--source-breakpoints nil
  "List of source-code breakpoints for current or future debugging sessions.

   Each entry is a cons pair of the form

   (FILENAME . (BREAKPOINTS))

   where filename is the full path to a source file, and
   BREAKPOINTS is a list of plists which have the form given in
   https://microsoft.github.io/debug-adapter-protocol/specification#Types_SourceBreakpoint")

(defvar dapdbg-io-print-flag nil
  "Enables output of request & response messages to/from the DAP
   server (only useful for debugging this package)")

(defconst dapdbg--lldb-plist
  (list :type "lldb-dap"
        :command-line-sym 'dapdbg-gdb-command-line
        :launch-args-sym :arguments
        :stop-on-entry-sym :stopOnEntry))

(defconst dapdbg--gdb-plist
  (list :type "gdb-dap"
        :command-line-sym 'dapdbg-gdb-command-line
        :launch-args-sym :args
        :stop-on-entry-sym :stopAtBeginningOfMainSubprogram))

(defun dapdbg-quit ()
  "Shut down and disconnect from the currently running DAP server (if any)."
  (interactive)
  (if (process-live-p dapdbg--process)
      (kill-process dapdbg--process)))

(defun dapdbg-start-lldb (program &optional program-arguments)
  (interactive "fProgram to launch: \nsProgram arguments: ")
  (dapdbg--start program program-arguments dapdbg--lldb-plist))

(defun dapdbg-start-gdb (program &optional program-arguments)
  (interactive "fProgram to launch: \nsProgram arguments: ")
  (dapdbg--start program program-arguments dapdbg--gdb-plist))

(defun dapdbg--start (program program-arguments debugger-plist)
  (when (process-live-p dapdbg--process)
    (pcase (substring (downcase (read-string "A debugger is already running, terminate and replace it (y/n)? " "y")) 0 1)
      ("y" (dapdbg-disconnect))
      (_ (error "Aborted"))))
  (dapdbg--connect (eval (plist-get debugger-plist :command-line-sym)))
  
  (let ((args (list
               :name (file-name-nondirectory program)
               :type (plist-get debugger-plist :type)
               :request "launch"
               :program (expand-file-name program)
               (plist-get debugger-plist :stop-on-entry-sym) t)))
    (when (not (string-empty-p program-arguments))
      (let ((tokens (string-split program-arguments)))
        (plist-put args (plist-get debugger-plist :launch-args-sym) tokens))) 
    (put 'dapdbg--process 'launch-args args)
    (dapdbg--initialize)))

(defun dapdbg-continue ()
  (interactive)
  (dapdbg--send-request "continue"
                        (list :threadId (get 'dapdbg--process :thread-id))))

(defun dapdbg-set-breakpoint (&optional filename line)
  (interactive)
  (unless filename
    (setq filename (expand-file-name (buffer-file-name))))
  (unless line
    (setq line (line-number-at-pos)))
  (dapdbg--send-request "setBreakpoints"
                        (list :source (list :name (file-name-nondirectory filename) :path filename)
                              :breakpoints (vector (list :line line)))))

(defun dapdbg-stacktrace (&optional thread-id)
  (interactive)
  (let ((tid thread-id))
    (unless tid
      (setq tid (get 'dapdbg--process 'thread-id)))
    (dapdbg--send-request "stackTrace"
                          (list :threadId tid))))

(defun dapdbg--enrich-process-symbol ()
  (put 'dapdbg--process 'callbacks (make-hash-table))
  (put 'dapdbg--process 'seq 1))

(defun dapdbg--connect (command-line)
  (setq dapdbg--process (make-process
                         :name "scp/debug-session"
                         :connection-type 'pipe
                         :coding 'no-conversion
                         :command command-line
                         :stderr "*debugger stderr*"
                         :filter #'dapdbg--handle-message
                         :noquery t))
  (dapdbg--enrich-process-symbol))

(defun dapdbg--ready-p ()
  (and (process-live-p dapdbg--process)
       (get 'dapdbg--process 'config-done)))

(defun dapdbg--handle-initialize-response (msg)
  (let ((caps (gethash "body" msg)))
    (dolist (cap '("supportsConditionalBreakpoints" "supportsConfigurationDoneRequest" "supportsDisassembleRequest"))
      (unless (gethash cap caps)
        (dapdbg-disconnect)
        (error "Debugger does not have capability \"%s\"" cap)))
    (put 'dapdbg--process 'capabilites caps))
  (let ((args (get 'dapdbg--process 'launch-args)))
                                        ;(dapdbg--set-all-breakpoints)
    (dapdbg--send-request "launch" args nil t)))

(defun dapdbg--initialize ()
  (let ((args (list
               :clientID "dapdbg"
               :clientName "dapdbg"
               :adapterID "dapdbg"
               :pathFormat "path"
               :linesStartAt1 t
               :columnsStartAt1 t
               :supportsVariableType t
               :supportsVariablePaging t
               :supportsRunInTerminalRequest t
               :locale "en-us")))
    (dapdbg--send-request "initialize" args #'dapdbg--handle-initialize-response t)))

(defun dapdbg--callbacks ()
  (get 'dapdbg--process 'callbacks))

(defun dapdbg--handle-response-configdone (msg)
  (put 'dapdbg--process 'config-done t))

(defun dapdbg--handle-event-stopped (msg)
  (let ((tid (gethash "threadId" (gethash "body" msg))))
    (put 'dapdbg--process 'thread-id tid)))

(defun dapdbg--handle-response (parsed-msg)
  (let* ((cb-seq (gethash "request_seq" parsed-msg))
         (cb-table (dapdbg--callbacks))
         (cb (gethash cb-seq cb-table)))
    (when cb
      (remhash cb-seq cb-table)
      (funcall cb parsed-msg))))

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

(defvar dapdbg--process nil
  "Global (could be directory-local) variable holding the debugger process")

(defun dapdbg--handle-event (parsed-msg)
  (pcase (gethash "event" parsed-msg)
    ("initialized"
     (dapdbg--send-request "configurationDone" nil #'dapdbg--handle-response-configdone t))
    ("stopped"
     (dapdbg--handle-event-stopped parsed-msg)
     (run-hook-with-args dapdbg--stopped-callback-list parsed-msg))
    ("continued" (run-hook-with-args dapdbg--continued-callback-list parsed-msg))
    ("output" (run-hook-with-args dapdbg--output-callback-list parsed-msg))
    ("thread" (run-hook-with-args dapdbg--thread-callback-list parsed-msg))
    (`,an-event (message "event: %s" an-event))))

(defun dapdbg--handle-message (_process msg)
  (let ((remaining msg)
        (chunks (list)))
    (while (not (string-empty-p remaining))
      (pcase-let ((`(:parsed-length ,length :parsed-msg ,parsed-msg) (dapdbg--parse-message remaining)))
        (pcase (gethash "type" parsed-msg)
          ("event" (dapdbg--handle-event parsed-msg))
          ("response" (dapdbg--handle-response parsed-msg))
          (`,something (error "unrecognized message type %s" something)))
        (add-to-list 'chunks length)
        (setq remaining (substring remaining length))))
    (reverse chunks)))

(defun dapdbg--seq-and-inc ()
  (let ((seq-value (get 'dapdbg--process 'seq)))
    (put 'dapdbg--process 'seq (1+ seq-value))
    seq-value))

(defun dapdbg--base-protocol (msg-plist seq)
  ;; https://microsoft.github.io/debug-adapter-protocol/overview#base-protocol
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
  (puthash seq callback (dapdbg--callbacks)))

(defun dapdbg--send-request (command &optional args callback skip-ready-check)
  (unless (or skip-ready-check (dapdbg--ready-p))
    (error "Debugger is not initialized"))
  (pcase-let ((`(:header ,hdrs :body ,body :seq ,seq) (dapdbg--create-request command args)))
    (dapdbg--io-message hdrs body t)
    (if callback
        (dapdbg--register-callback seq callback))
    (process-send-string dapdbg--process (format "%s\r\n%s" hdrs body))))

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

(defun dapdbg--parse-message (msg)
  (let ((end-of-header (string-search "\r\n\r\n" msg)))
    (unless end-of-header
      (error "malformed message from debugger"))
    (let* ((hdrs (substring msg 0 (+ end-of-header 2)))
           (content-length (dapdbg--parse-header (substring msg 0 end-of-header)))
           (beg (+ end-of-header 4))
           (end (+ beg content-length))
           (body (substring msg beg end)))
      (dapdbg--io-message hdrs body nil)
      (list
       :parsed-length end
       :parsed-msg (dapdbg--parse-json (substring body 0 content-length))))))

(defun dapdbg--io-buf ()
  "Buffer to display request/response messages if dapdbg-print-io is t"
  (let* ((buf-name "*dapdbg IO*")
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (js-json-mode)
        (font-lock-mode -1)))
    buf))

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
      )))

(provide 'dapdbg)

;;; dabdbg.el ends here
