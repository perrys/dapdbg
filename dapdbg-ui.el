;; dapdbg-ui.el --- User Interface for dapdbg -*- lexical-binding: t; -*-

(require 'dapdbg)

(defvar-keymap dapdbg-ui-mode-map
  :doc "Keymap for basic debugger controls."
  "<f1>" #'dapdbg-toggle-breakpoint
  "<f2>" #'dapdbg-next
  "C-<f2>" #'dapdbg-nexti
  "<f3>" #'dapdbg-step
  "C-<f3>" #'dapdbg-stepi
  "S-<f3>" #'dapdbg-finish
  "<f4>" #'dapdbg-continue
  )

(define-minor-mode dapdbg-ui-mode
  "Toggle dapdbg-ui-mode.

This minor mode is for display of active debugger information
such as the current line/instruction and breakpoint
information. It includes a keymap for basic debugger control."
  :init-value nil
  :lighter " dbg"
  (if dapdbg-ui-mode
      (dapdbg-ui-mode--enable)
    (dapdbg-ui-mode--disable)))

(defun dapdbg-ui-start-lldb (command-line)
  (interactive (list (read-shell-command "Target (command-line): ")))
  (dapdbg--start-lldb command-line))

(defun dapdbg-ui-start-gdb (command-line)
  (interactive (list (read-shell-command "Target (command-line): ")))
  (dapdbg--start-gdb command-line))

(defun dapdbg-ui-mode--enable ()
  (make-variable-buffer-local 'dapdbg-ui--buffer-breakpoints)
  (when (< left-margin-width 2)
    (dapdbg-ui--set-left-margin 2)))

(defun dapdbg-ui-mode--disable ()
  (dapdbg-ui--set-left-margin 0))

(defvar dapdbg--source-breakpoints-alist nil
  "List of source-code breakpoints for current or future debugging sessions.

   Each entry is a cons pair of the form

   (FILENAME . (BREAKPOINTS))

   where filename is the full path to a source file, and
   BREAKPOINTS is a list of plists which have the form given in
   https://microsoft.github.io/debug-adapter-protocol/specification#Types_SourceBreakpoint")

(defun dapdbg-ui--set-left-margin (width)
  (setq left-margin-width width)
  (let ((window (get-buffer-window (current-buffer) 0)))
    (if window
        (set-window-margins
         window left-margin-width right-margin-width))))

(defface dapdbg-ui-marker-face
  '((t :inherit (secondary-selection)))
  "Face for current line marker")

(defface dapdbg-ui-arrow-face
  '((t :inherit (warning)))
  "Face for current line marker arrow")

(defface dapdbg-ui-breakpoint-face
  '((t :inherit (error)))
  "Face for breakpoint markers")

(defvar dapdbg-ui--marker-overlay nil)

(defun dapdbg-ui--make-margin-marker-properties (&optional breakpoint-p)
  (list (list 'margin 'left-margin) 
        (propertize (if breakpoint-p ">" "=>") 'face 'dapdbg-ui-arrow-face)))
  
(defun dapdbg-ui--make-marker-overlay (start end buf)
  (let ((olay (make-overlay start end))
        (invisible-str (make-string 1 ?x)))
    (put-text-property 0 1 'display (dapdbg-ui--make-margin-marker-properties) invisible-str)
    (overlay-put olay :kind 'marker)
    (overlay-put olay 'face 'dapdbg-ui-marker-face)
    (overlay-put olay 'before-string invisible-str)
    olay))

(defun dapdbg-ui-mode--set-marker (buf linenumber)
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- linenumber))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (breakpointp (cl-some (lambda (olay) (eq (overlay-get olay :kind) 'breakpoint))
                                     (overlays-in bol eol))))
          (if dapdbg-ui--marker-overlay
              (move-overlay dapdbg-ui--marker-overlay bol eol buf) 
            (setq dapdbg-ui--marker-overlay
                  (dapdbg-ui--make-marker-overlay bol eol buf)))
          (put-text-property 0 1 'display (dapdbg-ui--make-margin-marker-properties breakpointp)
                             (overlay-get dapdbg-ui--marker-overlay 'before-string))))))
  (display-buffer buf))

(defvar-keymap dapdbg-ui-stacktrace-mode-map
  :doc "Local keymap for `Buffer-menu-mode' buffers."
  :parent tabulated-list-mode-map
  "RET" #'dapdbg-ui--switch-stackframe)

(define-derived-mode dapdbg-ui-stacktrace-mode tabulated-list-mode "STrace"
  "Major mode for stack trace display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Prog Counter" 12 nil :right-align t)
                '("Function" 999 nil)))
  (tabulated-list-init-header))


(defun dapdbg-ui--stacktrace-refresh (stacktrace)
  (let ((buf-created (dapdbg--get-or-create-buffer "*Stack*"))) 
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (dapdbg-ui-stacktrace-mode)
        (font-lock-mode -1)))
    (with-current-buffer (car buf-created)
      (setq tabulated-list-entries
            (mapcar
             (lambda (frame)
               (let ((id (gethash "id" frame))
                     (iptr (substring (gethash "instructionPointerReference" frame) 2))
                     (name (gethash "name" frame)))
                 (list id (vector
                           (propertize iptr 'face 'font-lock-number-face) 
                           (propertize name 'face 'font-lock-function-name-face)))))
             stacktrace))
      (tabulated-list-print))
    (display-buffer (car buf-created))))

(defvar-keymap dapdbg-ui-locals-mode-map
  :doc "Local keymap for `dapdbg-ui-locals-mode' buffers."
  :parent tabulated-list-mode-map
  "TAB" #'dapdbg-ui--expand-variable)

(define-derived-mode dapdbg-ui-locals-mode tabulated-list-mode "Var"
  "Major mode for local variables display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Name" 12 nil :right-align nil)
                '("Value" 999 nil)))
  (tabulated-list-init-header))

(define-derived-mode dapdbg-ui-registers-mode tabulated-list-mode "Reg"
  "Major mode for registers display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Name" 12 nil :right-align nil)
                '("Value" 999 nil)))
  (tabulated-list-init-header))

(defun dapdbg-ui--add-to-variables-tree (thing-list parent-id root-list-sym id-sym var-map)
  (let ((parent (gethash parent-id var-map)))
    (dolist (thing thing-list)
      (let ((id (set id-sym (1+ (symbol-value id-sym)))))
        (puthash id thing var-map)
        (puthash :id id thing)
        (puthash :children (list) thing))
      (if parent
          (push thing (gethash :children parent))
        (add-to-list root-list-sym thing)))))

(defun dapdbg-ui--make-tablulated-list-entries-r (node depth target-list)
  (let* ((my-children (gethash :children node))
         (switch (cond ((not (seq-empty-p my-children)) "- ")
                       ((> (gethash "variablesReference" node) 0) "+ ")
                       (t "  ")))
         (pad-fmt (format "%%%ds%%s%%s" (* 2 depth)))
         (name (format pad-fmt "" switch (gethash "name" node)))
         (value (format "%s" (gethash "value" node)))
         (id (gethash :id node))
         (entry (list id (vector
                          (propertize name 'face 'font-lock-variable-name-face) 
                          (propertize value 'face 'font-lock-number-face)))))
    (when my-children
      (dolist (child my-children)
        (setq target-list (dapdbg-ui--make-tablulated-list-entries-r child (1+ depth) target-list))))
    (push entry target-list)
    target-list))

(defun dapdbg-ui--make-tablulated-list-entries (nodes)
  (let ((target-list (list)))
    (dolist (node nodes)
      (let ((result (dapdbg-ui--make-tablulated-list-entries-r node 0 (list))))
        (if target-list
            (nconc target-list result)
          (setq target-list result))))
    target-list))


(defun dapdbg-ui--variables-refresh (buf-name mode thing-list parent-id &optional reset-flag)
  (let ((buf-created (dapdbg--get-or-create-buffer buf-name))) 
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (funcall mode)
        (font-lock-mode -1)
        ;; do this after setting the major mode
        (setq-local
         var-counter 0
         var-root-list (list)
         var-map (make-hash-table :test 'equal))))
    (with-current-buffer (car buf-created)
      (when reset-flag
        (setq-local
         var-counter 0
         var-root-list (list))
        (clrhash var-map))
      (dapdbg-ui--add-to-variables-tree thing-list parent-id 'var-root-list 'var-counter var-map)
      (setq tabulated-list-entries (dapdbg-ui--make-tablulated-list-entries var-root-list))
      (tabulated-list-print))
    (display-buffer (car buf-created))))

(defun dapdbg-ui--expand-variable (&optional parent-id)
  (interactive)
  (let* ((parent-id (or parent-id (tabulated-list-get-id)))
         (parent (gethash parent-id var-map))
         (var-id (gethash "variablesReference" parent))
         (processor #'dapdbg-ui--locals-update)
         (handler (lambda (parsed-msg1)
                    (let ((vars (gethash "variables" (gethash "body" parsed-msg1))))
                      (funcall processor parent-id vars)))))
    (if (> (length (gethash :children parent)) 0)
        (progn
          (puthash :children (list) parent)
          (setq tabulated-list-entries (dapdbg-ui--make-tablulated-list-entries var-root-list))
          (tabulated-list-print))
      (if (> var-id 0)
          (dapdbg--variables var-id handler)
        (warn "variable is not expandable")))))


(defmacro dapdbg-ui--make-tabulated-list-update (short-name docstring buf-name)
  `(defun ,(intern (format "dapdbg-ui--%s-update" short-name)) (parent-id thing-list &optional reset-flag)
     ,docstring
     (dapdbg-ui--variables-refresh ,buf-name ',(intern (format "dapdbg-ui-%s-mode" short-name)) thing-list parent-id reset-flag)))

(dapdbg-ui--make-tabulated-list-update
 "locals"
 "Buffer to display local variables"
 "*Locals*")

(dapdbg-ui--make-tabulated-list-update
 "registers"
 "Buffer to display register list"
 "*Registers*")

(defun dapdbg-ui--handle-scopes (parsed-msg &optional parent-id)
  (let ((scopes (gethash "scopes" (gethash "body" parsed-msg))))
    (dolist (scope scopes)
      (let ((kind (or (gethash "name" scope) (gethash "presentationHint" scope)))
            (id (gethash "variablesReference" scope)))
        (let ((processor
               (pcase (downcase kind)
                 ("locals" #'dapdbg-ui--locals-update)
                 ("registers" #'dapdbg-ui--registers-update)
                 (`,something
                  (message "unknown scope type: %s" something)
                  nil))))
          (when processor
            (let ((handler (lambda (parsed-msg1)
                             (let ((vars (gethash "variables" (gethash "body" parsed-msg1))))
                               (funcall processor parent-id vars t)))))
              (dapdbg--variables id handler))))))))

(defun dapdbg-ui--handle-stacktrace (parsed-msg)
  (let* ((stack (gethash "stackFrames" (gethash "body" parsed-msg)))
         (ip-ref (gethash "instructionPointerReference" (car stack)))
         (frame-id (gethash "id" (car stack)))
         (source (gethash "source" (car stack))))
    (dapdbg--scopes frame-id #'dapdbg-ui--handle-scopes)
    (when source
      (let* ((filename (gethash "path" source))
             (linenumber (gethash "line" (car stack)))
             (buf (find-file filename)))
        (with-current-buffer buf
          (dapdbg-ui-mode t))
        (dapdbg-ui-mode--set-marker buf linenumber)))
    (dapdbg-ui--stacktrace-refresh stack)))

(defun dapdbg-ui--handle-stopped-event (_parsed-msg)
  (dapdbg-stacktrace nil #'dapdbg-ui--handle-stacktrace))

(add-hook 'dapdbg--stopped-callback-list #'dapdbg-ui--handle-stopped-event)

(defun dapdbg-ui--handle-breakpoint-event (parsed-msg)
  (let* ((data (gethash "body" parsed-msg))
         (reason (gethash "reason" data)))
    (pcase reason
      ("new" (dapdbg-ui--mark-breakpoint data))
                                        ;("changed" (dapdbg-ui--refresh-breakpoints (gethash "path" (gethash "source" data))))
      (`,something (message "breakpoint reason: %s" something)))))

(add-hook 'dapdbg--breakpoint-callback-list #'dapdbg-ui--handle-breakpoint-event)

(defun dapdbg-ui--draw-breakpoint-marker (filename linenumber verified)
  (with-current-buffer (find-file filename)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- linenumber))
        (let ((marker (if verified "B" "b")))
          (let ((olay (make-overlay (point) (point)))
                (invisible-str (make-string 1 ?x))
                (marker-display-properties
                 (list (list 'margin 'left-margin) 
                       (propertize marker 'face 'dapdbg-ui-breakpoint-face))))
            (put-text-property 0 1 'display marker-display-properties invisible-str)
            (overlay-put olay 'before-string invisible-str)
            (overlay-put olay :kind 'breakpoint)
            olay))))))

(defun dapdbg-ui--mark-breakpoint (bp)
  (let* ((bpinfo (gethash "breakpoint" bp))
         (linenumber (gethash "line" bpinfo))
         (verified (gethash "verified" bpinfo))
         (source (gethash "source" bpinfo))
         (filename (gethash "path" source)))
    (dapdbg-ui--draw-breakpoint-marker filename linenumber verified)))

(defun dapdbg-ui-setup-many-windows ()
  (add-to-list
   'display-buffer-alist
   '((major-mode . dapdbg-ui-stacktrace-mode)
     (display-buffer-in-side-window)
     (side . left)
     (slot . 1)))
  (add-to-list
   'display-buffer-alist
   '((major-mode . dapdbg-ui-registers-mode)
     (display-buffer-in-side-window)
     (side . right)
     (slot . 2)))
  (add-to-list
   'display-buffer-alist
   '((major-mode . dapdbg-ui-locals-mode)
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1))))

(provide 'dapdbg-ui)
