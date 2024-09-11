;; dapdbg-ui.el bottom User Interface for dapdbg -*- lexical-binding: t; -*-


(require 'dapdbg)

;; ------------------- faces ---------------------

(defface dapdbg-ui-marker-face
  '((t :inherit (secondary-selection)))
  "Face for current line marker")

(defface dapdbg-ui-arrow-face
  '((t :inherit (warning)))
  "Face for current line marker arrow")

(defface dapdbg-ui-breakpoint-face
  '((t :inherit (error)))
  "Face for breakpoint markers")

;; ------------------- modes ---------------------

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

(defun dapdbg-ui-mode--enable ()
  (make-variable-buffer-local 'dapdbg-ui--buffer-breakpoints)
  (when (< left-margin-width 2)
    (dapdbg-ui--set-left-margin 2)))

(defun dapdbg-ui-mode--disable ()
  (dapdbg-ui--set-left-margin 0))

(defvar-keymap dapdbg-ui-variables-mode-map
  :doc "Local keymap for `dapdbg-ui-variables-mode' buffers."
  :parent tabulated-list-mode-map
  "TAB" #'dapdbg-ui--toggle-expand-variable)

(defvar-keymap dapdbg-ui-locals-mode-map :parent dapdbg-ui-variables-mode-map)

(define-derived-mode dapdbg-ui-locals-mode tabulated-list-mode "Var"
  "Major mode for local variables display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Name" 12 nil :right-align nil)
                '("Type" 16 nil)
                '("Value" 999 nil)))
  (tabulated-list-init-header))

(defvar-keymap dapdbg-ui-registers-mode-map :parent dapdbg-ui-variables-mode-map)

(define-derived-mode dapdbg-ui-registers-mode tabulated-list-mode "Reg"
  "Major mode for registers display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Name" 12 nil :right-align nil)
                '("Type" 16 nil)
                '("Value" 999 nil)))
  (tabulated-list-init-header))

(defvar-keymap dapdbg-ui-stacktrace-mode-map
  :doc "Local keymap for `STrace' buffers."
  :parent tabulated-list-mode-map
  "RET" #'dapdbg-ui--switch-stackframe)

(define-derived-mode dapdbg-ui-stacktrace-mode tabulated-list-mode "STrace"
  "Major mode for stack trace display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Prog Counter" 12 nil :right-align t)
                '("Function" 999 nil)))
  (tabulated-list-init-header))

(defvar-keymap dapdbg-ui-output-mode-map
  :doc "Local keymap for `dapdbg I/O' buffers."
  "RET" #'dapdbg-ui--send-input)

(define-derived-mode dapdbg-ui-output-mode fundamental-mode "dbgIO"
  "Major mode for the debugger REPL and process output"
  :interactive nil
  (setq-local dapdbg-ui--output-mark (point-min)
              dapdbg-ui--io-prompt (propertize "(gdb)" 'read-only t 'face 'comint-highlight-prompt))
  (insert (concat dapdbg-ui--io-prompt " "))
  (setq-local dapdbg-ui--input-mark (point-max)))

;; ------------------- commands ---------------------

(defun dapdbg-ui-start-lldb (command-line)
  (interactive (list (read-shell-command "Target (command-line): ")))
  (dapdbg--start-lldb command-line))

(defun dapdbg-ui-start-gdb (command-line)
  (interactive (list (read-shell-command "Target (command-line): ")))
  (dapdbg--start-gdb command-line))

(defun dapdbg-ui--eval-repl (expr)
  (interactive "sExpression: ")
  (dapdbg--eval-repl
   expr
   (lambda (parsed-msg)
     (dapdbg-ui--output (format "> %s\n" expr) "repl-input")
     (dapdbg-ui--output (gethash "result" (gethash "body" parsed-msg))))))

;; ------------------- margin stuff ---------------------

(defvar dapdbg-ui--marker-overlay nil)

(defun dapdbg-ui--set-left-margin (width)
  (setq left-margin-width width)
  (let ((window (get-buffer-window (current-buffer) 0)))
    (if window
        (set-window-margins
         window left-margin-width right-margin-width))))

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
  (unless (get-buffer-window buf "visible")
    (display-buffer buf)))

(defun dapdbg-ui--draw-breakpoint-marker (buf linenumber verified)
  (with-current-buffer buf
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

(defun dapdbg-ui--clear-breakpoint-markers (buf)
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let ((bp-overlays (cl-remove-if-not (lambda (olay) (eq (overlay-get olay :kind) 'breakpoint))
                                           (overlays-in (point-min) (point-max)))))
        (dolist (olay bp-overlays) (delete-overlay olay))))))

;; ------------------- input/output ---------------------

(defun dapdbg-ui--output (data &optional category)
  (let* ((buf-created (dapdbg--get-or-create-buffer "*Input/Output*"))
         (buf (car buf-created)))
    (when (cdr buf-created)
      (with-current-buffer buf
        (dapdbg-ui-output-mode)
        (font-lock-mode -1)))
    (with-current-buffer buf
      (let ((props (list 'read-only t)))
        (pcase category
              ("stderr" (plist-put props 'face 'font-lock-warning-face))
              ("repl-input" (plist-put props 'face 'font-lock-comment-face))
              ("repl-output" (plist-put props 'face 'font-lock-string-face)))
        (add-text-properties 0 (length data) props data)
        (goto-char dapdbg-ui--output-mark)
        (let ((inhibit-read-only t)) (insert data))
        (setq-local dapdbg-ui--output-mark (+ dapdbg-ui--output-mark (length data))
                    dapdbg-ui--input-mark (+ dapdbg-ui--input-mark (length data)))
        (goto-char (point-max))))
    (display-buffer buf)))

(defun dapdbg-ui--send-input ()
  (interactive)
  (let ((input (buffer-substring-no-properties dapdbg-ui--input-mark (point-max))))
    (delete-region dapdbg-ui--input-mark (point-max))
    (dapdbg-ui--eval-repl input)))

;; ------------------- stacktrace ---------------------

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

;; ------------------- variables & registers ---------------------

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
         (type (format "%s" (gethash "type" node)))
         (value (format "%s" (gethash "value" node)))
         (id (gethash :id node))
         (entry (list id (vector
                          (propertize name 'face 'font-lock-variable-name-face) 
                          (propertize type 'face 'font-lock-type-face) 
                          (propertize value 'face 'font-lock-string-face)))))
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
            (setq target-list (nconc result target-list))
          (setq target-list result))))
    target-list))

(defun dapdbg-ui--variables-refresh (buf-name mode processor thing-list parent-id &optional reset-flag)
  (let ((buf-created (dapdbg--get-or-create-buffer buf-name))) 
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (funcall mode)
        (font-lock-mode -1)
        ;; do this after setting the major mode
        (setq-local
         var-processor processor
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
      (let ((point (point)))
        (tabulated-list-print)
        (goto-char point)))
    (display-buffer (car buf-created))))

(defun dapdbg-ui--un-expand-variable (parent)
  (puthash :children (list) parent)
  (setq tabulated-list-entries (dapdbg-ui--make-tablulated-list-entries var-root-list))
  (let ((point (point)))
    (tabulated-list-print)
    (goto-char point)))

(defun dapdbg-ui--toggle-expand-variable (&optional parent-id)
  "Expand or un-expand the variable for the row at point in the tabulated-list."
  (interactive)
  (unless var-map
    (error "not in a variables window"))
  (let* ((parent-id (or parent-id (tabulated-list-get-id)))
         (parent (gethash parent-id var-map))
         (var-id (gethash "variablesReference" parent))
         (handler (lambda (parsed-msg1)
                    (let ((vars (gethash "variables" (gethash "body" parsed-msg1))))
                      (funcall var-processor parent-id vars)))))
    (if (> (length (gethash :children parent)) 0)
        (dapdbg-ui--un-expand-variable parent)
      (if (> var-id 0)
          (dapdbg--variables var-id handler)
        (warn "variable is not expandable")))))

(defmacro dapdbg-ui--make-variables-update-fn (short-name docstring buf-name)
  (let ((fn-symbol (intern (format "dapdbg-ui--%s-update" short-name))))
    `(defun ,fn-symbol (parent-id thing-list &optional reset-flag)
       ,docstring
       (dapdbg-ui--variables-refresh ,buf-name '
                                     ,(intern (format "dapdbg-ui-%s-mode" short-name))
                                     ',fn-symbol
                                     thing-list parent-id reset-flag))))

(dapdbg-ui--make-variables-update-fn
 "globals"
 "Buffer to display global variables"
 "*Globals*")

(dapdbg-ui--make-variables-update-fn
 "locals"
 "Buffer to display local variables"
 "*Locals*")

(dapdbg-ui--make-variables-update-fn
 "registers"
 "Buffer to display register list"
 "*Registers*")

;; ------------------- breakpoints ---------------------

(defun dapdbg-ui--set-breakpoint-markers (buf source-table)
  (dapdbg-ui--clear-breakpoint-markers buf)
  (with-current-buffer buf
    (dolist (linenumber (hash-table-keys source-table))
      (let ((bp-details (gethash linenumber source-table)))
        (if (hash-table-p bp-details)
            (dapdbg-ui--draw-breakpoint-marker buf linenumber (gethash "verified" bp-details))
          (dapdbg-ui--draw-breakpoint-marker buf linenumber nil))))))

(defun dapdbg-ui--refresh-breakpoints (filename bp-table)
  (let ((buf (get-file-buffer filename)))
    (when buf
      (dapdbg-ui--set-breakpoint-markers buf bp-table))))

;; ------------------- callbacks ---------------------

(defun dapdbg-ui--handle-scopes-response (parsed-msg)
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
                               (funcall processor nil vars t)))))
              (dapdbg--variables id handler))))))))

(defun dapdbg-ui--handle-stacktrace-response (parsed-msg)
  (let* ((stack (gethash "stackFrames" (gethash "body" parsed-msg)))
         (ip-ref (gethash "instructionPointerReference" (car stack)))
         (frame-id (gethash "id" (car stack)))
         (source (gethash "source" (car stack))))
    (dapdbg--scopes frame-id #'dapdbg-ui--handle-scopes-response)
    (when source
      (let* ((filename (gethash "path" source))
             (linenumber (gethash "line" (car stack)))
             (buf (find-file-noselect filename)))
        (with-current-buffer buf
          (dapdbg-ui-mode t))
        (dapdbg-ui-mode--set-marker buf linenumber)))
    (dapdbg-ui--stacktrace-refresh stack)))

(defun dapdbg-ui--handle-output-event (parsed-msg)
  (let ((body (gethash "body" parsed-msg)))
    (dapdbg-ui--output (gethash "output" body) (gethash "category" body))))

(add-hook 'dapdbg--output-callback-list #'dapdbg-ui--handle-output-event)

(defun dapdbg-ui--handle-stopped-event (_parsed-msg)
  (dapdbg--stacktrace nil #'dapdbg-ui--handle-stacktrace-response))

(add-hook 'dapdbg--stopped-callback-list #'dapdbg-ui--handle-stopped-event)

(defun dapdbg-ui--handle-breakpoints-updated (updates-table)
  (maphash #'dapdbg-ui--refresh-breakpoints updates-table))

(add-hook 'dapdbg--breakpoints-updated-callback-list #'dapdbg-ui--handle-breakpoints-updated)

;; ------------------- layout ---------------------

(defun dapdbg-ui-setup-many-windows ()
  (interactive)
  (add-to-list
   'display-buffer-alist
   '((major-mode . dapdbg-ui-output-mode)
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)))
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
     (slot . 3))))

(provide 'dapdbg-ui)
