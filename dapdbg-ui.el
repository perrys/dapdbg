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

(defun dapdbg-ui--handle-stacktrace (parsed-msg)
  (let* ((stack (gethash "stackFrames" (gethash "body" parsed-msg)))
         (ipRef (gethash "instructionPointerReference" (car stack)))
         (source (gethash "source" (car stack))))
    (when source
      (let* ((filename (gethash "path" source))
            (linenumber (gethash "line" (car stack)))
            (buf (find-file filename)))
        (with-current-buffer buf
          (dapdbg-ui-mode t))
        (dapdbg-ui-mode--set-marker buf linenumber)))))

        

(defun dapdbg-ui--handle-stopped-event (_parsed-msg)
  (dapdbg-stacktrace nil #'dapdbg-ui--handle-stacktrace))

                           
                           
  
(add-hook 'dapdbg--stopped-callback-list #'dapdbg-ui--handle-stopped-event)
          
(defun dapdbg-ui--set-source-breakpoint (filename line enabled bp-number)
  (save-excursion
    (with-current-buffer (find-file filename)
      (gdb-put-breakpoint-icon enabled bp-number line))))





