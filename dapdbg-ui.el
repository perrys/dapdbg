(require 'dapdbg)

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

(defvar dapdbg-ui--marker-overlay nil)

(defun dapdpg-ui--make-marker-overlay (start end buf)
  (let ((olay (make-overlay start end))
        (invisible-str (make-string 1 ?x))
        (marker-display-properties
         (list (list 'margin 'left-margin) 
               (propertize "=>" 'face 'dapdbg-ui-arrow-face))))
    (put-text-property 0 1 'display marker-display-properties invisible-str)
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
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (if dapdbg-ui--marker-overlay
              (move-overlay dapdbg-ui--marker-overlay bol eol buf) 
            (setq dapdbg-ui--marker-overlay
                  (dapdpg-ui--make-marker-overlay bol eol buf))))))))

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
      (tabulated-list-print))))

(define-derived-mode dapdbg-ui-registers-mode tabulated-list-mode "Reg"
  "Major mode for registers display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Name" 8 nil :right-align nil)
                '("Value" 999 nil)))
  (tabulated-list-init-header))

(defun dapdbg-ui--registers-refresh (registers)
  (let ((buf-created (dapdbg--get-or-create-buffer "*Registers*"))) 
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (dapdbg-ui-registers-mode)
        (font-lock-mode -1)))
    (with-current-buffer (car buf-created)
      (setq tabulated-list-entries
            (mapcar
             (lambda (frame)
               (let ((name (gethash "name" frame))
                     (value (gethash "value" frame)))
                 (list name (vector
                           (propertize name 'face 'font-lock-variable-name-face) 
                           (propertize value 'face 'font-lock-number-face)))))
             registers))
      (tabulated-list-print))))

(defun dapdbg-ui--handle-locals (parsed-msg)
  nil)

(defun dapdbg-ui--handle-registers (parsed-msg)
  (let ((registers (gethash "variables" (gethash "body" parsed-msg))))
    (dapdbg-ui--registers-refresh registers)))
    


(defun dapdbg-ui--handle-scopes (parsed-msg)
  (let ((scopes (gethash "scopes" (gethash "body" parsed-msg))))
    (dolist (scope scopes)
      (let ((kind (or (gethash "name" scope) (gethash "presentationHint" scope)))
            (id (gethash "variablesReference" scope)))
        (pcase (downcase kind)
          ("locals" (dapdbg--variables id #'dapdbg-ui--handle-locals))
          ("registers" (dapdbg--variables id #'dapdbg-ui--handle-registers))
          (`,something (message "unknown scope type: %s" something)))))))
            
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
          
(defun dapdbg-ui--set-source-breakpoint (filename line enabled bp-number)
  (save-excursion
    (with-current-buffer (find-file filename)
      (gdb-put-breakpoint-icon enabled bp-number line))))





