;; dapdbg-ui.el bottom User Interface for dapdbg -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stewart Perry

;; Author: Stewart Perry <stewart.c.perry@gmail.com>
;; Created: 31 Aug 2024

;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This file is free software

;;; Commentary:

;; Add-on package for dapdbg providing a UI for interactive debugging. The UI
;; has several panels (in addition to the source buffer) for native debugging,
;; similar in nature to those found in the Windows' debugger `windbg', or emacs
;; built-int GUD debugger with `gdb-many-windows' enabled:
;;
;; * Breakpoints
;; * Stack Frames
;; * Disassembly
;; * Variables
;; * Registers
;; * Memory View
;;
;; This UI only uses built-in features of emacs. In particular, it makes heavy
;; use of tabulated-list-mode for the majority of the display panels above.

;;; Code:

(require 'dapdbg)

;; ------------------- faces ---------------------

(defface dapdbg-ui-marker-face
  '((t :inherit (secondary-selection)))
  "Face for current line marker"
  :group 'dapdbg)

(defface dapdbg-ui-arrow-face
  '((t :inherit (warning)))
  "Face for current line marker arrow"
  :group 'dapdbg)

(defface dapdbg-ui-breakpoint-face
  '((t :inherit (error)))
  "Face for breakpoint markers"
  :group 'dapdbg)

;; ------------------- custom variables ---------------------

(defcustom dapdbg-ui-input-ring-size 64
  "Length of command-history for the I/O buffer"
  :group 'dapdbg
  :type 'natnum)

(defcustom dapdbg-ui-address-format "%013x"
  "Format for addresses to be printed in various panels.

The default is 13 hex characters (zero-padded), which is enough
to display 52-bit addresses. You may need to widen this for some
architectures."
  :group 'dapdbg
  :type 'natnum)

(defcustom dapdbg-ui-show-globals-flag t
  "Show global variables in the variables buffer.

Some debugged programs report a large number of global variables
which are not interesting from the point of view of the
debuggging process. This flag can be used to supress the fetch
and display of those."
  :group 'dapdbg
  :type 'boolean)

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
  "C-<f4>" #'dapdbg-pause
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

;; ------------------- commands ---------------------

(defun dapdbg-ui-start-lldb (command-line)
  (interactive (list (read-shell-command "Target (command-line): ")))
  (dapdbg--start-lldb command-line))

(defun dapdbg-ui-attach-lldb (pid &optional program)
  (interactive "nProcess ID (pid): ")
  (unless program
    (setq program (read-shell-command "Target program (optional): ")))
  (dapdbg--attach-lldb pid (if (string-empty-p program) nil program)))

(defun dapdbg-ui-start-codelldb (command-line)
  (interactive (list (read-shell-command "Target (command-line): ")))
  (dapdbg--start-codelldb command-line))

(defun dapdbg-ui-start-gdb (command-line)
  (interactive (list (read-shell-command "Target (command-line): ")))
  (dapdbg--start-gdb command-line))

(defun dapdbg-ui--eval-repl (expr)
  (interactive "sExpression: ")
  (dapdbg--request-eval-repl
   expr
   (lambda (parsed-msg)
     (dapdbg-ui--output (format "> %s\n" expr) "repl-input")
     (dapdbg-ui--output (gethash "result" (gethash "body" parsed-msg))))))

;; ------------------- marker and margin stuff ---------------------

;; Displaying something in the margin is not straightforward - if you just add a
;; margin property to the existing buffer text on that line, the text will
;; dissapear (it is "replaced" by the margin property). The suggested workaround
;; in the elisp manual is to create a (possibly invisible) overlay on that line,
;; set a `before-string' property on the overlay, and set the margin property on
;; *that* string. The contents of the before-string are therefore not important,
;; because it will never actually be displayed.
;;
;; Because we already have an overlay for the source/instruction line, we can
;; re-use it for the margin display trick.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Margins.html

(defvar dapdbg-ui--source-line-marker-overlay nil)
(defvar dapdbg-ui--instruction-marker-overlay nil)

(defun dapdbg-ui--set-left-margin (width)
  "Display a margin of width WIDTH in the current buffer."
  (setq left-margin-width width)
  (let ((window (get-buffer-window (current-buffer) 0)))
    (if window
        (set-window-margins
         window left-margin-width right-margin-width))))

(defun dapdbg-ui--make-margin-display-properties (margin-str)
  "Make a display specification (a nested list) to display something in the margin."
  (list (list 'margin 'left-margin) margin-str))

(defun dapdbg-ui--make-overlay-for-margin (start end buf kind &optional face)
  "Make an overlay which has the `before-string' property."
  (let ((olay (make-overlay start end))
        (invisible-str (make-string 1 ?x)))
    (overlay-put olay 'before-string invisible-str)
    (overlay-put olay :kind kind)
    (when face
      (overlay-put olay 'face face))
    olay))

(defun dapdbg-ui--set-margin-str-for-overlay (olay margin-str)
  (let ((invisible-str (overlay-get olay 'before-string)))
    (put-text-property 0 1 'display
                       (dapdbg-ui--make-margin-display-properties margin-str)
                       invisible-str)))

(defun dapdbg-ui--make-marker-overlay (start end buf)
  (dapdbg-ui--make-overlay-for-margin start end buf 'marker 'dapdbg-ui-marker-face))

(defun dapdbg-ui-mode--set-instruction-marker (buf)
  "Move the instruction marker to the current point in BUF."
  (unless dapdbg-ui--instruction-marker-overlay
    (setq dapdbg-ui--instruction-marker-overlay
          (dapdbg-ui--make-marker-overlay 0 0 buf)))
  (dapdbg-ui-mode--set-marker-at-point buf dapdbg-ui--instruction-marker-overlay))

(defun dapdbg-ui-mode--set-source-line-marker (buf linenumber)
  "Move the source-line marker to the given line number in BUF."
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- linenumber))
        (unless dapdbg-ui--source-line-marker-overlay
          (setq dapdbg-ui--source-line-marker-overlay
                (dapdbg-ui--make-marker-overlay 0 0 buf)))
        (dapdbg-ui-mode--set-marker-at-point buf dapdbg-ui--source-line-marker-overlay)))))

(defun dapdbg-ui-mode--set-marker-at-point (buf marker-olay)
  "Moves the given marker overlay to the current point in BUF,
accounting for existing breakpoint markers."
  (let ((bol (line-beginning-position)))
    (let* ((eol (line-end-position))
           (bp-flag (cl-some (lambda (olay) (eq (overlay-get olay :kind) 'breakpoint))
                             (overlays-in bol eol)))
           (arrow-str (propertize (if bp-flag ">" "=>") 'face 'dapdbg-ui-arrow-face)))
      (move-overlay marker-olay bol eol buf)
      (dapdbg-ui--set-margin-str-for-overlay marker-olay arrow-str))
    (unless (get-buffer-window buf "visible")
      (display-buffer buf))
    (with-selected-window (get-buffer-window buf)
      (unless (pos-visible-in-window-p bol)
        (goto-char bol)
        (recenter)))))

(defun dapdbg-ui--draw-breakpoint-marker (buf linenumber verified)
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- linenumber))
        (let ((bp-str (propertize (if verified "B" "b") 'face 'dapdbg-ui-breakpoint-face))
              (olay (dapdbg-ui--make-overlay-for-margin (point) (point) buf 'breakpoint)))
          (dapdbg-ui--set-margin-str-for-overlay olay bp-str)
          olay)))))

(defun dapdbg-ui--clear-breakpoint-markers (buf)
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let ((bp-overlays (cl-remove-if-not (lambda (olay) (eq (overlay-get olay :kind) 'breakpoint))
                                           (overlays-in (point-min) (point-max)))))
        (dolist (olay bp-overlays) (delete-overlay olay))))))

;; ------------------- REPL and I/O ---------------------

(defconst dapdbg-ui--interaction-buffer-name "*dapdbg REPL*")

(defvar-keymap dapdbg-ui-output-mode-map
  :doc "Local keymap for `dapdbg I/O' buffers."
  :parent dapdbg-ui-mode-map
  "M-p" #'dapdbg-ui-previous-input
  "M-n" #'dapdbg-ui-next-input
  "RET" #'dapdbg-ui-send-input)

(define-derived-mode dapdbg-ui-output-mode fundamental-mode "dbg"
  "Major mode for the debugger REPL and process output"
  :interactive nil)

(defun dapdbg-ui--get-repl-buffer ()
  "Get the main buffer used to interact with the debugger."
  (let* ((buf-created (dapdbg--get-or-create-buffer dapdbg-ui--interaction-buffer-name))
         (buf (car buf-created)))
    (when (cdr buf-created)
      (with-current-buffer buf
        (dapdbg-ui-output-mode)
        (setq-local dapdbg-ui--progress-table (make-hash-table :test 'eql)
                    dapdbg-ui--prompt-olay (make-overlay (point-max) (point-max))
                    dapdbg-ui--input-ring-index nil
                    dapdbg-ui--incomplete-input nil
                    dapdbg-ui--input-ring (make-ring dapdbg-ui-input-ring-size))
        (font-lock-mode -1)))
    buf))

(defun dapdbg-ui--set-repl-prompt ()
  "Set up the repl prompt using the type of the current session debugger."
  (with-current-buffer (dapdbg-ui--get-repl-buffer)
    (overlay-put dapdbg-ui--prompt-olay
                 'before-string (propertize (format "(%s) " (dapdbg-session-type dapdbg--ssn))
                                            'face 'comint-highlight-prompt))
    (setq-local mode-name (format "dbg: %s" (dapdbg-session-target-name dapdbg--ssn)))))

(defun dapdbg-ui--output (data &optional category)
  "Output the given data into the repl buffer just before the prompt."
  (when (not (string-empty-p data))
    (let ((buf (dapdbg-ui--get-repl-buffer)))
      (with-current-buffer buf
        (goto-char (overlay-start dapdbg-ui--prompt-olay))
        (dapdbg-ui--output-at-point data category)
        (goto-char (point-max)))
      (display-buffer buf))))

(defun dapdbg-ui--output-at-point (data &optional category)
  (let ((inhibit-read-only t)
        (len (length data))
        (face (pcase category
                ("stderr" 'font-lock-warning-face)
                ('event 'font-lock-comment-face)
                ('progress 'font-lock-comment-face))))
    (when face
      (put-text-property 0 (1- len) 'face face data))
    (insert-before-markers (propertize (substring data 0 (1- len)) 'read-only t))
    (insert-before-markers (propertize (substring data (1- len) len) 'rear-nonsticky t 'read-only t))))

(defun dapdbg-ui--progress (id percent &optional message)
  "Output a progress indicator into the current buffer. The first
progess update for ID is inserted into the buffer just before the
prompt, and subsequent updates are written to the same line."
  (with-current-buffer (dapdbg-ui--get-repl-buffer)
    (let ((progress-record (gethash id dapdbg-ui--progress-table))
          (output-mark (overlay-start dapdbg-ui--prompt-olay)))
      (unless progress-record
        (setq progress-record (puthash id (cons (output-mark) message) dapdbg-ui--progress-table)))
      (if message
          (setcdr progress-record message)
        (setq message (cdr progress-record)))
      (goto-char (car progress-record))
      (delete-region (point) (line-end-position))
      (let ((text (format "# Progress: %s %d%%\n" message percent)))
        (dapdbg-ui--output-at-point text 'progress)))))

(defun dapdbg-ui-previous-input (arg)
  (interactive "*p")
  (if (ring-empty-p dapdbg-ui--input-ring)
      (message "no previous command")
    (let ((input-mark (overlay-end dapdbg-ui--prompt-olay)))
      (unless dapdbg-ui--input-ring-index
        (let ((current-input (buffer-substring-no-properties input-mark (point-max))))
          (unless (string-empty-p current-input)
            (setq-local dapdbg-ui--incomplete-input current-input)))
        (setq-local dapdbg-ui--input-ring-index 0))
      (let ((prev-input (ring-ref dapdbg-ui--input-ring dapdbg-ui--input-ring-index)))
        (cl-incf dapdbg-ui--input-ring-index)
        (goto-char input-mark)
        (delete-region (point) (point-max))
        (insert prev-input)))))

(defun dapdbg-ui-next-input (arg)
  (interactive "*p")
  (when dapdbg-ui--input-ring-index
    (let ((next-input
           (if (eql dapdbg-ui--input-ring-index 0)
               (progn
                 (setq dapdbg-ui--input-ring-index nil)
                 (or dapdbg-ui--incomplete-input ""))
             (cl-decf dapdbg-ui--input-ring-index)
             (ring-ref dapdbg-ui--input-ring dapdbg-ui--input-ring-index))))
      (goto-char (overlay-end dapdbg-ui--prompt-olay))
      (delete-region (point) (point-max))
      (insert next-input))))

(defun dapdbg-ui-send-input ()
  (interactive)
  (let* ((input-mark (overlay-end dapdbg-ui--prompt-olay))
         (input (buffer-substring-no-properties input-mark (point-max))))
    (when (not (string-empty-p (string-trim input)))
      (delete-region input-mark (point-max))
      (ring-insert dapdbg-ui--input-ring input)
      (setq-local dapdbg-ui--input-ring-index nil)
      (dapdbg-ui--eval-repl input))))

;; ------------------- call-stack ---------------------

(defconst dapdbg-ui--call-stack-buffer-name "*Stack Frames*")

(defvar-keymap dapdbg-ui-call-stack-mode-map
  :doc "Local keymap for `STrace' buffers."
  :parent (make-composed-keymap dapdbg-ui-mode-map tabulated-list-mode-map)
  "RET" #'dapdbg-ui--select-stack-frame)

(define-derived-mode dapdbg-ui-call-stack-mode tabulated-list-mode "stack"
  "Major mode for stack trace display"
  :interactive nil
  (let ((addr-width (length (format dapdbg-ui-address-format 0))))
    (setq tabulated-list-format
          (vector '("Idx" 3 nil :right-align t)
                  `("Prog Counter" ,(if (> addr-width 1) addr-width 16) nil)
                  '("Function" 999 nil))))
  (tabulated-list-init-header))

(defun dapdbg-ui--get-call-stack-buffer ()
  (let ((buf-created (dapdbg--get-or-create-buffer dapdbg-ui--call-stack-buffer-name)))
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (dapdbg-ui-call-stack-mode)
        (setq-local buffer-read-only t)
        (font-lock-mode -1)
        (make-variable-buffer-local 'current-thread-id)
        (make-variable-buffer-local 'current-call-stack)
        (setq-local marker-overlay (make-overlay 0 0))
        (overlay-put marker-overlay :kind 'frame-marker)
        (overlay-put marker-overlay 'face 'dapdbg-ui-marker-face)))
    (car buf-created)))

(defun dapdbg-ui--call-stack-display-update (call-stack)
  "Update the table display in the call-stack buffer."
  (let ((buf (dapdbg-ui--get-call-stack-buffer))
        (idx -1))
    (with-current-buffer buf
      (setq tabulated-list-entries
            (mapcar
             (lambda (frame)
               (cl-incf idx)
               (let ((id (gethash "id" frame))
                     (pc (gethash "instructionPointerReference" frame))
                     (name (gethash "name" frame)))
                 (when pc
                   (setq pc (dapdbg--parse-address pc)))
                 (list id (vector
                           (propertize (format "%d" idx) 'face 'font-lock-variable-name-face)
                           (if pc (dapdbg-ui--addr pc) "<unknown>")
                           (propertize name 'face 'font-lock-function-name-face)))))
             call-stack))
      (tabulated-list-print)
      (goto-char (point-min))
      (display-buffer buf))))

(defun dapdbg-ui--set-call-stack (thread-id call-stack)
  "Set the current thread-id and call-stack, usually after a `stopped'
event. This causes a chain of updates to occur in various panels."
  (with-current-buffer (dapdbg-ui--get-call-stack-buffer)
    (setq-local
     current-thread-id thread-id
     current-call-stack call-stack))
  (dapdbg-ui--call-stack-display-update call-stack)
  (dapdbg-ui--set-call-stack-context call-stack))

(defun dapdbg-ui--select-stack-frame ()
  "Select the frame at point in the call-stack buffer."
  (interactive)
  (with-current-buffer (dapdbg-ui--get-call-stack-buffer)
    (let ((frame-id (tabulated-list-get-id))
          (call-stack current-call-stack))
      (while (and call-stack (/= frame-id (gethash "id" (car call-stack))))
        (setq call-stack (cdr call-stack)))
      (unless call-stack
        (error "unable to find frame-id %d in current stack" frame-id))
      (dapdbg-ui--set-call-stack-context call-stack)
      (move-overlay marker-overlay (line-beginning-position) (line-end-position)))))

(defun dapdbg-ui--set-call-stack-context (call-stack)
  "Adjust various panels to show the context of the given call
stack, which may be a partially-unwound portion of the stack that
the debugee is current stopped at."
  ;; create a "stack path" from the current call-stack to serve as an ID for caching variables etc:
  (let* ((call-stack-id (string-join (nreverse (mapcar (lambda (f) (gethash "name" f)) call-stack)) "/"))
         (frame (car call-stack))
         (ip-ref (gethash "instructionPointerReference" frame)))
    (dapdbg-ui--reset-variables-buffers call-stack-id)
    (dapdbg--request-scopes (gethash "id" frame)
                            (apply-partially #'dapdbg-ui--handle-scopes-response call-stack-id))
    (when ip-ref
      (dapdbg-ui--handle-program-counter-updated (dapdbg--parse-address ip-ref)))
    (when-let ((source (gethash "source" frame)))
      (let ((filename (gethash "path" source))
            (linenumber (gethash "line" frame)))
        (when (and filename (file-exists-p filename))
          (with-current-buffer (find-file-noselect filename)
            (dapdbg-ui-mode t)
            (dapdbg-ui-mode--set-source-line-marker (current-buffer) linenumber))))
      ;; TODO - remove marker if it was not set
      )))

;; ------------------- variables & registers ---------------------

(defconst dapdbg-ui--variables-buffer-name "*Variables*")
(defconst dapdbg-ui--registers-buffer-name "*Registers*")

(defvar-keymap dapdbg-ui-variables-mode-map
  :doc "Local keymap for `dapdbg-ui-variables-mode' buffers."
  :parent (make-composed-keymap dapdbg-ui-mode-map tabulated-list-mode-map)
  "TAB" #'dapdbg-ui--toggle-expand-variable)

(define-derived-mode dapdbg-ui-variables-mode tabulated-list-mode "vars"
  "Major mode for local variables display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Name" 16 nil :right-align nil)
                '("Type" 16 nil)
                '("Value" 999 nil)))
  (tabulated-list-init-header))

(cl-defstruct dapdbg-ui--var-tree
  "Holds the tree structure of variables data. The hash table holds
all variable objects (for the frame at the top of a particular
call stack) keyed by their path in the tree, and the tree
structure is formed via the `:child-ids` property of each
variable data object, which point to entries in the hash table."
  (root-ids (list))
  (table (make-hash-table :test 'equal)))

(defun dapdbg-ui--get-var (id var-tree)
  "Convenience fn to get a variable's data from the tree structure"
  (gethash id (dapdbg-ui--var-tree-table var-tree)))

(defun dapdbg-ui--add-to-variables-tree (parent-id child-list var-tree)
  "Add variables from CHILD-LIST to the tree structure held in
VAR-TREE. Return the subset of ids from CHILD-LIST which had
children at the last stop-point, and therefore need additional
reloading."
  (let* ((var-table (dapdbg-ui--var-tree-table var-tree))
         (parent (gethash parent-id var-table))
         (reload-ids (list)))
    (dolist (child-var child-list)
      (let* ((id (concat parent-id "/" (gethash "name" child-var)))
             (old-entry (gethash id var-table)))
        (puthash id child-var var-table)
        (puthash :valid t child-var)
        (puthash :id id child-var)
        (puthash :child-ids (list) child-var)
        (when (and old-entry (gethash :child-ids old-entry))
          (let ((var-ref (gethash "variablesReference" child-var)))
            (when (> var-ref 0)
              (push (cons id var-ref) reload-ids))))
        (if parent
            (push id (gethash :child-ids parent))
          (unless (cl-find id (dapdbg-ui--var-tree-root-ids var-tree) :test 'equal) ;; TODO - I think this is O(N^2)
            (push id (dapdbg-ui--var-tree-root-ids var-tree))))))
    reload-ids))

(defun dapdbg-ui--startswith (needle haystack)
  (equal needle
         (downcase (substring haystack 0 (length needle)))))

(defun dapdbg-ui--escape (str)
  (mapconcat (lambda (c) (if (eql c ?\n) "\\n" (list c))) str ""))

(defun dapdbg-ui--make-variables-tabulated-list-r (id var-table depth target-list)
  "Construct a list for display by recursing through the tree
structure starting from the node in VAR-TABLE with key ID,
pushing each item to TARGET-LIST."
  (let* ((node (gethash id var-table))
         (my-children (gethash :child-ids node))
         (valid (gethash :valid node))
         (type-val (gethash "type" node))
         (display-type (if (and (eql depth 0) (dapdbg-ui--startswith "global" id))
                           (format "<glbl> %s" type-val)
                         type-val))
         (switch (cond ((not (seq-empty-p my-children)) "- ")
                       ((> (gethash "variablesReference" node) 0) "+ ")
                       (t "  ")))
         (pad-fmt (format "%%%ds%%s%%s" (* 2 depth)))
         (name (format pad-fmt "" switch (gethash "name" node)))
         (type (format "%s" (if valid display-type "<invalid>")))
         (value (format "%s" (if valid (dapdbg-ui--escape (gethash "value" node)) "_")))
         (entry (list id (vector
                          (propertize name 'face 'font-lock-variable-name-face)
                          (propertize type 'face 'font-lock-type-face)
                          (propertize value 'face 'font-lock-string-face)))))
    (when my-children
      (dolist (child-id my-children)
        (setq target-list (dapdbg-ui--make-variables-tabulated-list-r child-id var-table (1+ depth) target-list))))
    (cons entry target-list)))

(defun dapdbg-ui--make-variables-tabulated-list (var-tree)
  "Make a list of the form required for `tabulated-list-entries' from
the root IDs list and hash table of VAR-TREE"
  (let ((target-list (list))
        (root-ids (dapdbg-ui--var-tree-root-ids var-tree))
        (var-map (dapdbg-ui--var-tree-table var-tree)))
    (dolist (id root-ids)
      (setq target-list (dapdbg-ui--make-variables-tabulated-list-r id var-map 0 target-list)))
    target-list))

(defun dapdbg-ui--un-expand-variable (parent)
  "Set an empty list for the children of PARENT."
  (puthash :child-ids (list) parent)
  (dapdbg-ui--refresh-variables-display))

(defun dapdbg-ui--refresh-variables-display ()
  "Render variables in the current tabulated-list buffer."
  (let ((var-tree (dapdbg-ui--get-tree-for-call-stack current-call-stack-id)))
    (setq tabulated-list-entries (dapdbg-ui--make-variables-tabulated-list var-tree))
    (let ((point (point)))
      (tabulated-list-print)
      (goto-char point))))

(defun dapdbg-ui--toggle-expand-variable ()
  "Expand or un-expand the variable for the row at point in the tabulated-list
of the current buffer."
  (interactive)
  (unless tree-table
    (error "not in a variables buffer"))
  (let* ((parent-id (tabulated-list-get-id))
         (var-tree (dapdbg-ui--get-tree-for-call-stack current-call-stack-id))
         (parent (dapdbg-ui--get-var parent-id var-tree))
         (var-id (gethash "variablesReference" parent))
         (handler (lambda (parsed-msg1)
                    (let ((vars (gethash "variables" (gethash "body" parsed-msg1))))
                      (dapdbg-ui--variables-refresh current-call-stack-id parent-id vars (buffer-name))))))
    (if (> (length (gethash :child-ids parent)) 0)
        (dapdbg-ui--un-expand-variable parent)
      (if (> var-id 0)
          (dapdbg--request-variables var-id handler)
        (warn "variable is not expandable")))))

(defun dapdbg-ui--get-tree-for-call-stack (call-stack-id)
  "Get the dapdbg-ui--var-tree structure for the given call-stack, via
the local hash table for the current buffer."
  (let ((tree (gethash call-stack-id tree-table)))
    (unless tree
      (setq tree (puthash call-stack-id (make-dapdbg-ui--var-tree) tree-table)))
    tree))

(defun dapdbg-ui--reload-child-vars (reload-ids)
  "Request variable data for the variables specified in RELOAD-IDS,
each of which is a cons-pair of parent-path and the reference id
provided by the DAP server."
  (unless current-call-stack-id
    (error "not in a variables buffer"))
  (let ((call-stack-id current-call-stack-id)
        (buf-name (buffer-name)))
    (dolist (reload reload-ids)
      (let* ((parent-id (car reload))
             (var-ref (cdr reload))
             (handler (lambda (parsed-msg)
                        (let ((vars (gethash "variables" (gethash "body" parsed-msg))))
                          (dapdbg-ui--variables-refresh call-stack-id parent-id vars buf-name)))))
        (dapdbg--request-variables var-ref handler)))))

(defun dapdbg-ui--get-variables-buffer (buf-name)
  (let ((buf-created (dapdbg--get-or-create-buffer buf-name)))
    (when (cdr buf-created) ; freshly-opened variables buffer
      (with-current-buffer (car buf-created)
        (dapdbg-ui-variables-mode)
        (font-lock-mode -1)
        ;; do this after setting the major mode
        (setq-local
         current-call-stack-id nil
         tree-table (make-hash-table :test 'equal))))
    (car buf-created)))

(defun dapdbg-ui--variables-refresh (call-stack-id parent-id child-list buf-name)
  "Update the tree structure for CALL-STACK-ID with children of variable
PARENT-ID provided in CHILD-LIST."
  (let ((buf (dapdbg-ui--get-variables-buffer buf-name)))
    (with-current-buffer buf
      (let* ((var-tree (dapdbg-ui--get-tree-for-call-stack call-stack-id))
             (reload-ids (dapdbg-ui--add-to-variables-tree parent-id child-list var-tree)))
        (if reload-ids
            (dapdbg-ui--reload-child-vars reload-ids)
          (dapdbg-ui--refresh-variables-display)))) ; at a leaf node
    (display-buffer buf)))

(defun dapdbg-ui--registers-update (_call-stack-id parent-id child-list)
  "Update registers buffer with (partial) tree data"
  (with-current-buffer dapdbg-ui--registers-buffer-name
    (setq-local current-call-stack-id "registers"))
  (dapdbg-ui--variables-refresh "registers" parent-id child-list
                                dapdbg-ui--registers-buffer-name))

(defun dapdbg-ui--variables-update (call-stack-id parent-id child-list)
  "Update variables buffer with (partial) tree data"
  (dapdbg-ui--variables-refresh call-stack-id parent-id child-list
                                dapdbg-ui--variables-buffer-name))

(defun dapdbg-ui--invalidate-variables-buffer (buf call-stack-id)
  "Invalidate the root of the variables tree - normally called from a stopped event."
  (when-let ((var-tree (dapdbg-ui--get-tree-for-call-stack call-stack-id)))
    (let ((var-map (dapdbg-ui--var-tree-table var-tree)))
      (dolist (id (dapdbg-ui--var-tree-root-ids var-tree))
        (let ((var-obj (gethash id var-map)))
          (puthash :valid nil var-obj))))))

(defun dapdbg-ui--reset-variables-buffers (call-stack-id)
  (dolist (buf-name '(dapdbg-ui--variables-buffer-name dapdbg-ui--registers-buffer-name))
    (let ((buf (dapdbg-ui--get-variables-buffer (symbol-value buf-name))))
      (with-current-buffer buf
        (if (equal buf-name dapdbg-ui--registers-buffer-name)
            (setq-local current-call-stack-id "registers")
          (setq-local current-call-stack-id call-stack-id)
          (dapdbg-ui--invalidate-variables-buffer buf call-stack-id))))))

(defun dapdbg-ui--handle-scopes-response (call-stack-id parsed-msg)
  (let ((scopes (gethash "scopes" (gethash "body" parsed-msg))))
    (dolist (scope scopes)
      (let ((kind (or (gethash "name" scope) (gethash "presentationHint" scope)))
            (id (gethash "variablesReference" scope)))
        (let ((processor
               (pcase (downcase kind)
                 ("registers" #'dapdbg-ui--registers-update)
                 ("globals" (if dapdbg-ui-show-globals-flag #'dapdbg-ui--variables-update nil))
                 (_ #'dapdbg-ui--variables-update))))
          (when processor
            (let ((handler (lambda (parsed-msg1)
                             (let ((vars (gethash "variables" (gethash "body" parsed-msg1))))
                               (funcall processor call-stack-id kind vars)))))
              (dapdbg--request-variables id handler))))))))

;; ------------------- disassembly ---------------------

(defconst dapdbg-ui--disassembly-buffer-name "*Disassembly*")

(defvar-keymap dapdbg-ui-asm-mode-map
  :doc "Local keymap for disassembly buffers."
  :parent dapdbg-ui-mode-map)

(define-derived-mode dapdbg-ui-asm-mode asm-mode "disassembly"
  "Major mode for the debugger's disassembly buffer"
  :interactive nil)

(defun dapdbg-ui--get-disassembly-buffer ()
  (let ((buf-created (dapdbg--get-or-create-buffer dapdbg-ui--disassembly-buffer-name)))
    (when (cdr buf-created) ; freshly-opened
      (with-current-buffer (car buf-created)
        (dapdbg-ui-asm-mode)
        (dapdbg-ui--set-left-margin 2)
        (font-lock-mode -1)
        ;; do this after setting the major mode
        (setq-local
         address-bol-map (make-hash-table :test 'eql)
         instruction-cache (make-hash-table :test 'eql))))
    (car buf-created)))

(defun dapdbg-ui--mapchain (table key key-prop &optional max-count already-reversed-flag)
  "Return the list of elements from TABLE following the list implied
by the KEY-PROP property of each element, starting at KEY."
  (let ((result nil)
        (counter 0))
    (while-let ((current (and (or (null max-count) (< counter max-count))
                              (gethash key table))))
      (setq result (cons current result))
      (setq key (plist-get current key-prop))
      (setq counter (1+ counter)))
    (if already-reversed-flag
        result
      (nreverse result))))

(defun dapdbg-ui--link-instructions (addr last-instruction current-instruction)
  (plist-put last-instruction :next addr)
  (plist-put current-instruction :prev (plist-get last-instruction :addr))
  ;; Remove symbol data from the current instruction if it is identical to the
  ;; previous one (e.g. codelldb outputs the same function symbol for every
  ;; instruction in a function)
  (let ((last (plist-get last-instruction :data))
        (current (plist-get current-instruction :data)))
    (let ((last-symbol (or (gethash "_symbol" last) (gethash "symbol" last)))
          (current-symbol (or (gethash "_symbol" current) (gethash "symbol" current))))
      (when (string-equal last-symbol current-symbol)
        (puthash "_symbol" last-symbol current)
        (remhash "symbol" current)))))

(defun dapdbg-ui--update-instruction-cache (instructions i-cache)
  "Add INSTRUCTIONS to the cache, with each entry in the
cache forming a doubly-linked list to the previous and next
instruction addresses. Stop if the instruction list overlaps with
an existing set of instructions, and join the linked-lists at
that point)."
  (let ((last-instruction nil)
        (current-instruction nil)
        (started-new-list-flag nil))
    (catch 'joined-to-existing-list
      (dolist (instruction instructions)
        (setq last-instruction current-instruction)
        (let ((addr (dapdbg--parse-address (gethash "address" instruction))))
          (if-let ((existing-instruction (gethash addr i-cache)))
              (progn
                (setq current-instruction existing-instruction)
                (when last-instruction
                  (dapdbg-ui--link-instructions addr last-instruction current-instruction))
                (when started-new-list-flag
                  (throw 'joined-to-existing-list nil)))
            ;; this is a previously-unseen address
            (setq current-instruction
                  (list :addr addr :prev nil :next nil :data instruction))
            (setq started-new-list-flag t)
            (when last-instruction
              (dapdbg-ui--link-instructions addr last-instruction current-instruction))
            (puthash addr current-instruction i-cache)))))))

(defun dapdbg-ui--addr (address)
  (format dapdbg-ui-address-format address))

(defun dapdbg-ui--render-instruction-line (cache-entry)
  "Insert text for the instruction in CACHE-ENTRY into the current
buffer. Return the beginning of line mark."
  (let* ((pc (plist-get cache-entry :addr))
         (instruction (plist-get cache-entry :data))
         (line (format "%s: %s\n" (dapdbg-ui--addr pc) (gethash "instruction" instruction))))
    (if-let ((symbol (gethash "symbol" instruction)))
        (insert (propertize (format "\n%s <%s>:\n" (dapdbg-ui--addr pc) symbol) 'read-only t)))
    (let ((saved-point (point)))
      (insert (propertize line 'read-only t))
      saved-point)))

(defun dapdbg-ui--disassembly-render (program-counter)
  "Clear and repopulate the disassembly buffer with instructions
from the instruction cache around PROGRAM-COUNTER."
  (let ((buf (dapdbg-ui--get-disassembly-buffer)))
    (with-current-buffer buf
      (clrhash address-bol-map)
      (let ((inhibit-read-only t)
            (prev-instructions (dapdbg-ui--mapchain instruction-cache program-counter :prev 64 t))
            (instructions (dapdbg-ui--mapchain instruction-cache program-counter :next 64)))
        (erase-buffer)
        (dolist (instruction prev-instructions)
          (let ((addr (plist-get instruction :addr))
                (bol (dapdbg-ui--render-instruction-line instruction)))
            (puthash addr bol address-bol-map)))
        (dolist (instruction (cdr instructions))
          (let ((addr (plist-get instruction :addr))
                (bol (dapdbg-ui--render-instruction-line instruction)))
            (puthash addr bol address-bol-map))))
      (font-lock-fontify-buffer)
      (when-let ((marker-point (gethash program-counter address-bol-map)))
        (goto-char marker-point)
        (dapdbg-ui-mode--set-instruction-marker buf)))
    (display-buffer buf)))

(defun dapdbg-ui--handle-disassembly (pc parsed-msg)
  (let ((instructions (gethash "instructions" (gethash "body" parsed-msg)))
        (i-cache (with-current-buffer (dapdbg-ui--get-disassembly-buffer) instruction-cache)))
    (dapdbg-ui--update-instruction-cache instructions i-cache)
    (dapdbg-ui--disassembly-render pc)))

(defun dapdbg-ui--handle-program-counter-updated (program-counter)
  (when (gethash "supportsDisassembleRequest" (dapdbg-session-capabilities dapdbg--ssn))
    (with-current-buffer (dapdbg-ui--get-disassembly-buffer)
      (if-let ((marker-point (gethash program-counter address-bol-map)))
          (progn
            (goto-char marker-point)
            (dapdbg-ui-mode--set-instruction-marker (current-buffer)))
        (dapdbg--request-disassembly program-counter nil (apply-partially #'dapdbg-ui--handle-disassembly program-counter))))))

;; ------------------- memory view ---------------------

(defconst dapdbg-ui--memory-view-buffer-name "*Memory*")

(defun dapdbg-ui--get-memory-view-buffer ()
  (let ((buf-created (dapdbg--get-or-create-buffer dapdbg-ui--memory-view-buffer-name)))
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (dapdbg-ui-memory-view-mode)
        (setq-local buffer-read-only t)
        (font-lock-mode -1)))
    (car buf-created)))

(define-derived-mode dapdbg-ui-memory-view-mode tabulated-list-mode "mem"
  "Major mode for memory view"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Address" 12 nil)
                '("0011" 4 nil)
                '("2233" 4 nil)
                '("4455" 4 nil)
                '("6677" 4 nil)
                '("8899" 4 nil)
                '("aabb" 4 nil)
                '("ccdd" 4 nil)
                '("eeff" 4 nil)
                '("0123456789abcdef" 16 nil)))
  (tabulated-list-init-header))

(defun dapdbg-ui--mutate-to-ascii (bytes)
  (let ((i 0)
        (len (length bytes)))
    (while (< i len)
      (let ((c (aref bytes i)))
        (if (or (< c #x20) (> c ?~))
            (aset bytes i ?.)
          (cl-incf i)))))
  bytes)

(defun dapdbg-ui--render-line (addr bytes)
  (let ((line (make-vector 10 "")))
    (aset line 0 (format "%012x" addr))
    (aset line 1 (concat (format "%02x" (aref bytes 0)) (format "%02x" (aref bytes 1))))
    (aset line 2 (concat (format "%02x" (aref bytes 2)) (format "%02x" (aref bytes 3))))
    (aset line 3 (concat (format "%02x" (aref bytes 4)) (format "%02x" (aref bytes 5))))
    (aset line 4 (concat (format "%02x" (aref bytes 6)) (format "%02x" (aref bytes 7))))
    (aset line 5 (concat (format "%02x" (aref bytes 8)) (format "%02x" (aref bytes 9))))
    (aset line 6 (concat (format "%02x" (aref bytes 10)) (format "%02x" (aref bytes 11))))
    (aset line 7 (concat (format "%02x" (aref bytes 12)) (format "%02x" (aref bytes 13))))
    (aset line 8 (concat (format "%02x" (aref bytes 14)) (format "%02x" (aref bytes 15))))
    (aset line 9 (dapdbg-ui--mutate-to-ascii bytes))
    line))

(defun dapdbg-ui--display-memory (start data)
  (with-current-buffer (dapdbg-ui--get-memory-view-buffer)
    (let ((offset 0)
          (result nil))
      (while (< offset (length data))
        (setq result (cons (list (+ start offset) (dapdbg-ui--render-line (+ start offset) (substring data offset (+ offset 16)))) result))
        (setq offset (+ offset 16)))
      (setq tabulated-list-entries (nreverse result))
      (tabulated-list-print))))

(defun dapdbg-ui--refresh-memory-view (start end)
  (let* ((alignment 16)
         (mask (lognot (- alignment 1)))
         (start-aligned (logand mask start))
         (misalign (mod end alignment))
         (count  (- end start-aligned)))
    (when (> misalign 0)
      (setq count (logand mask (+ count alignment))))
    (dapdbg--request-memory-dump start-aligned count
                                 (lambda (parsed-msg)
                                   (let* ((b64-data (gethash "data" (gethash "body" parsed-msg)))
                                          (data (base64-decode-string b64-data)))
                                     (dapdbg-ui--display-memory start-aligned data))))))

;; ------------------- breakpoints ---------------------

(defconst dapdbg-ui--breakpoints-buffer-name "*Breakpoints*")

(defvar-keymap dapdbg-ui-breakpoints-mode-map
  :doc "Local keymap for `Breakpoints' buffers."
  :parent (make-composed-keymap dapdbg-ui-mode-map tabulated-list-mode-map))

(define-derived-mode dapdbg-ui-breakpoints-mode tabulated-list-mode "breaks"
  "Major mode for breakpoints display"
  :interactive nil
  (let ((addr-width (length (format dapdbg-ui-address-format 0))))
    (setq tabulated-list-format
          (vector
           '("ID" 2 nil :right-align t)
           '("Type" 4 nil)
           '("Vrf" 3 nil :right-align t)
           '("Enb" 3 nil :right-align t)
           `("Address" ,(if (> addr-width 1) addr-width 16) nil :right-align t)
           '("Hits" 6 nil :right-align t)
           '("What" 999 nil))))
  (tabulated-list-init-header))

(defun dapdbg-ui--make-breakpoint-line (bp-details)
  (let ((id (gethash "id" bp-details))
        (source (gethash "source" bp-details))
        (linenumber (gethash "line" bp-details))
        (addr (gethash "instructionReference" bp-details)))
    (list id
          (vector (format "%d" id)
                  (if source
                      "src"
                    "n/a")
                  (if (gethash "verified" bp-details) "yes" "no")
                  (cons "yes" nil)
                  (if addr
                      (format dapdbg-ui-address-format (dapdbg--parse-address addr))
                    "<unknown>")
                  (format "%d" (gethash "hits" bp-details))
                  (if-let ((filename (if source (gethash "name" source) nil)))
                      (format "%s:%d" (file-name-nondirectory filename) linenumber)
                    "")))))

(defun dapdbg-ui--breakpoints-display-update ()
  "Update the table display in the breakpoints buffer."
  (let ((buf-created (dapdbg--get-or-create-buffer dapdbg-ui--breakpoints-buffer-name)))
    (when (cdr buf-created)
      (with-current-buffer (car buf-created)
        (dapdbg-ui-breakpoints-mode)
        (setq-local buffer-read-only t)
        (font-lock-mode -1)))
    (with-current-buffer (car buf-created)
      (setq tabulated-list-entries nil)
      (maphash (lambda (id bp-details)
                 (push (dapdbg-ui--make-breakpoint-line bp-details)
                       tabulated-list-entries))
               (dapdbg-session-breakpoints dapdbg--ssn))
      (setq tabulated-list-entries (nreverse tabulated-list-entries))
      (tabulated-list-print)
      (goto-char (point-min))
      (display-buffer (car buf-created)))))

(defun dapdbg-ui--set-breakpoint-markers (buf source-table)
  (dapdbg-ui--clear-breakpoint-markers buf)
  (let ((table (dapdbg-session-breakpoints dapdbg--ssn)))
    (with-current-buffer buf
      (dolist (linenumber (hash-table-keys source-table))
        (let* ((id (gethash linenumber source-table))
               (bp-details (gethash id table)))
          (if (hash-table-p bp-details)
              (dapdbg-ui--draw-breakpoint-marker buf linenumber (gethash "verified" bp-details))
            (dapdbg-ui--draw-breakpoint-marker buf linenumber nil)))))))

(defun dapdbg-ui--breakpoint-hit (ids)
  (seq-doseq (id ids)
    (let ((bp-details (gethash id (dapdbg-session-breakpoints dapdbg--ssn))))
      (cl-incf (gethash "hits" bp-details)))))

(defun dapdbg-ui--refresh-source-breakpoints (filename bp-table)
  (let ((buf (get-file-buffer filename)))
    (when buf
      (dapdbg-ui--set-breakpoint-markers buf bp-table))))

;; ------------------- callbacks ---------------------

(defun dapdbg-ui--handle-stacktrace-response (thread-id parsed-msg)
  (let ((stack (gethash "stackFrames" (gethash "body" parsed-msg))))
    (dapdbg-ui--set-call-stack thread-id stack))
  (dapdbg-ui--breakpoints-display-update))

(defun dapdbg-ui--handle-threads-response (parsed-msg)
  nil) ;; TODO

(defun dapdbg-ui--handle-process-event (parsed-msg)
  (dapdbg-ui--set-repl-prompt)
  (let ((body (gethash "body" parsed-msg)))
    (dapdbg-ui--output (format "# Event: %s %s, pid: %d\n"
                               (gethash "startMethod" body)
                               (gethash "name" body)
                               (gethash "systemProcessId" body))
                       'event)))
(add-hook 'dapdbg--process-callback-list #'dapdbg-ui--handle-process-event)

(defun dapdbg-ui--handle-progress-event (parsed-msg)
  (let* ((body (gethash "body" parsed-msg))
         (msg (or (gethash "message" body)
                  (gethash "title" body)
                  nil))
         (id (gethash "id" body))
         (pct (or (gethash "percentage" body) "<na>")))
    (dapdbg-ui--progress id pct msg)))

(add-hook 'dapdbg--progressStart-list #'dapdbg-ui--handle-progress-event)
(add-hook 'dapdbg--progressUpdate-list #'dapdbg-ui--handle-progress-event)
(add-hook 'dapdbg--progressEnd-list #'dapdbg-ui--handle-progress-event)

(defun dapdbg-ui--handle-exited-event (parsed-msg)
  (dapdbg-ui--output "# Event: exited\n" 'event))
(add-hook 'dapdbg--exited-callback-list #'dapdbg-ui--handle-exited-event)

(defun dapdbg-ui--handle-terminated-event (parsed-msg)
  (dapdbg-ui--output "# Event: terminated\n" 'event))
(add-hook 'dapdbg--terminated-callback-list #'dapdbg-ui--handle-terminated-event)

(defun dapdbg-ui--handle-continued-event (parsed-msg)
  (unless (eq 'stepping (dapdbg-session-target-state dapdbg--ssn))
    (dapdbg-ui--output "# Event: continued\n" 'event)))
(add-hook 'dapdbg--continued-callback-list #'dapdbg-ui--handle-continued-event)

(defun dapdbg-ui--handle-output-event (parsed-msg)
  (let ((body (gethash "body" parsed-msg)))
    (dapdbg-ui--output (gethash "output" body) (gethash "category" body))))
(add-hook 'dapdbg--output-callback-list #'dapdbg-ui--handle-output-event)

(defun dapdbg-ui--handle-stopped-event (parsed-msg)
  (let ((target-state (dapdbg-session-target-state dapdbg--ssn)))
    (unless (eq 'stopped-step target-state)
      (let* ((body (gethash "body" parsed-msg))
             (ids (gethash "hitBreakpointIds" body))
             (reason (gethash "reason" body))
             (msg (or (gethash "description" body) reason)))
        (dapdbg-ui--output (format "# Event stopped (%s)\n" msg) 'event)
        (when (eq 'stopped-breakpoint target-state)
          (dapdbg-ui--breakpoint-hit ids)))))
  ;; current thread ID has already been set on the session object
  (dapdbg--request-stacktrace nil #'dapdbg-ui--handle-stacktrace-response)
  (dapdbg--request-threads #'dapdbg-ui--handle-threads-response))

(add-hook 'dapdbg--stopped-callback-list #'dapdbg-ui--handle-stopped-event)

(defun dapdbg-ui--handle-breakpoints-updated (updates-table)
  (maphash #'dapdbg-ui--refresh-source-breakpoints updates-table)
  (dapdbg-ui--breakpoints-display-update))

(add-hook 'dapdbg--breakpoints-updated-callback-list #'dapdbg-ui--handle-breakpoints-updated)

;; ------------------- layout ---------------------

(defun dapdbg-ui-setup-many-windows ()
  (interactive)
  (add-to-list
   'display-buffer-alist
   `(,dapdbg-ui--interaction-buffer-name
     (display-buffer-in-side-window)
     (side . left)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   `(,dapdbg-ui--call-stack-buffer-name
     (display-buffer-in-side-window)
     (side . left)
     (slot . 2)))
  (add-to-list
   'display-buffer-alist
   `(,dapdbg-ui--memory-view-buffer-name
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)))
  (add-to-list
   'display-buffer-alist
   `(,dapdbg-ui--disassembly-buffer-name
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)))
  (add-to-list
   'display-buffer-alist
   `(,dapdbg-ui--breakpoints-buffer-name
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 3)))
  (add-to-list
   'display-buffer-alist
   `(,dapdbg-ui--variables-buffer-name
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1)))
  (add-to-list
   'display-buffer-alist
   `(,dapdbg-ui--registers-buffer-name
     (display-buffer-in-side-window)
     (side . right)
     (slot . 2)))
  )

(provide 'dapdbg-ui)

;;; dabdbg-ui.el ends here
