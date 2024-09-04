(require 'dapdbg)
(require 'gdb-mi)

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
  (make-variable-buffer-local 'dapdbg-ui--marker-overlay)
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
  '((((class color)) :background "darkslategray")
    (t :weight bold))
  "Face for current line marker")

(defface dapdbg-ui-marker-arrow-face
  '((((class color)) :foreground "red" :background "reset")
    (t :weight bold))
  "Face for current line marker")

(defun dapdbg-ui-mode--set-marker (line)
  (save-excursion
    (save-restriction
      (widen)
    (goto-line line)
    (let ((olay (make-overlay
                 (line-beginning-position)
                 (line-end-position)))
          (invisible-str (make-string 1 ?x))
          (margin-arrow-display-properties
           (list (list 'margin 'left-margin) 
                 (propertize "->" 'face 'dapdbg-ui-marker-arrow-face))))
      (add-text-properties 0 1 (list
                                'display margin-arrow-display-properties)
                                invisible-str)
      (overlay-put olay 'face 'dapdbg-ui-marker-face)
      (overlay-put olay 'before-string invisible-str)))))

  


(defun dapdbg-ui--set-source-breakpoint (filename line enabled bp-number)
  (save-excursion
    (with-current-buffer (find-file filename)
      (gdb-put-breakpoint-icon enabled bp-number line))))





