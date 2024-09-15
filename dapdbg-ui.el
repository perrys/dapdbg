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

(define-derived-mode dapdbg-ui-variables-mode tabulated-list-mode "vars"
  "Major mode for local variables display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Name" 16 nil :right-align nil)
                '("Type" 16 nil)
                '("Value" 999 nil)))
  (tabulated-list-init-header))

(defvar-keymap dapdbg-ui-call-stack-mode-map
  :doc "Local keymap for `STrace' buffers."
  :parent tabulated-list-mode-map
  "RET" #'dapdbg-ui--select-stack-frame)

(define-derived-mode dapdbg-ui-call-stack-mode tabulated-list-mode "stack"
  "Major mode for stack trace display"
  :interactive nil
  (setq tabulated-list-format
        (vector '("Prog Counter" 12 nil :right-align t)
                '("Function" 999 nil)))
  (tabulated-list-init-header))

(defvar-keymap dapdbg-ui-output-mode-map
  :doc "Local keymap for `dapdbg I/O' buffers."
  "M-p" #'dapdbg-ui-previous-input
  "RET" #'dapdbg-ui-send-input)

(defcustom dapdbg-ui-input-ring-size 64
  "Length of command-history for the I/O buffer"
  :type 'integer)

(define-derived-mode dapdbg-ui-output-mode fundamental-mode "dbgIO"
  "Major mode for the debugger REPL and process output"
  :interactive nil
  (setq-local dapdbg-ui--output-mark (point-min)
              dapdbg-ui--io-prompt (propertize "(gdb)" 'read-only t 'face 'comint-highlight-prompt)
              dapdbg-ui--input-ring (make-ring dapdbg-ui-input-ring-size))
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
    (put-text-property 0 1 'display (dapdbg-ui--make-margin-display-properties margin-str) invisible-str)))

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

(defun dapdbg-ui-previous-input (arg)
  (interactive "*p")
  (if (ring-empty-p dapdbg-ui--input-ring)
      (message "no previous command")
    (unless dapdbg-ui--input-ring-index
      (let ((current-input (buffer-substring-no-properties dapdbg-ui--input-mark (point-max))))
        (unless (string-empty-p current-input)
          (setq-local dapdbg-ui--incomplete-input current-input)))
      (setq-local dapdbg-ui--input-ring-index 1))
    (let ((prev-input (ring-ref dapdbg-ui--input-ring dapdbg-ui--input-ring-index)))
      (cl-incf dapdbg-ui--input-ring-index)
      (delete-region dapdbg-ui--input-mark (point-max))
      (goto-char dapdbg-ui--input-mark)
      (insert prev-input))))

(defun dapdbg-ui-send-input ()
  (interactive)
  (let ((input (buffer-substring-no-properties dapdbg-ui--input-mark (point-max))))
    (delete-region dapdbg-ui--input-mark (point-max))
    (ring-insert dapdbg-ui--input-ring input)
    (setq-local dapdbg-ui--input-ring-index nil)
    (dapdbg-ui--eval-repl input)))

;; ------------------- call-stack ---------------------

(defconst dapdbg-ui--call-stack-buffer-name "*Stack Frames*")

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
  (let ((buf (dapdbg-ui--get-call-stack-buffer)))
    (with-current-buffer buf
      (setq tabulated-list-entries
            (mapcar
             (lambda (frame)
               (let ((id (gethash "id" frame))
                     (iptr (substring (gethash "instructionPointerReference" frame) 2))
                     (name (gethash "name" frame)))
                 (list id (vector
                           (propertize iptr 'face 'font-lock-number-face)
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
  (let ((call-stack-id (string-join (nreverse (mapcar (lambda (f) (gethash "name" f)) call-stack)) "/"))
        (frame (car call-stack)))
    (dapdbg-ui--invalidate-variables-buffers call-stack-id)
    (dapdbg--request-scopes (gethash "id" frame)
                            (apply-partially #'dapdbg-ui--handle-scopes-response call-stack-id))
    (when (gethash "supportsDisassembleRequest" (dapdbg-session-capabilities dapdbg--ssn))
      (let ((prog-counter (dapdbg--parse-address (gethash "instructionPointerReference" frame))))
        (dapdbg--request-disassembly prog-counter nil (apply-partially #'dapdbg-ui--handle-disassembly prog-counter))))
    (when-let ((source (gethash "source" frame)))
      (let ((filename (gethash "path" source))
            (linenumber (gethash "line" frame)))
        (when (file-exists-p filename)
          (with-current-buffer (find-file-noselect filename)
            (dapdbg-ui-mode t)
            (dapdbg-ui-mode--set-source-line-marker (current-buffer) linenumber))))
      ;; TODO - remove marker if it was not set
      )))

;; ------------------- variables & registers ---------------------

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
  (mapconcat (lambda (c) (if (eql c ?\n) "\\n" (list c))) str))

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

(defmacro dapdbg-ui--make-variables-update-fn (short-name docstring buf-name)
  (let ((fn-symbol (intern (format "dapdbg-ui--%s-update" short-name))))
    `(defun ,fn-symbol (call-stack-id parent-id child-list)
       ,docstring
       (dapdbg-ui--variables-refresh call-stack-id parent-id child-list
                                     ,buf-name))))

(defconst dapdbg-ui--variables-buffer-name "*Variables*")
(defconst dapdbg-ui--registers-buffer-name "*Registers*")

(dapdbg-ui--make-variables-update-fn
 "variables"
 "Update variables buffer with (partial) tree data"
 dapdbg-ui--variables-buffer-name)

(dapdbg-ui--make-variables-update-fn
 "registers"
 "Buffer to display register list"
 dapdbg-ui--registers-buffer-name)

(defun dapdbg-ui--invalidate-variables-buffer (buf call-stack-id)
  "Invalidate the root of the variables tree - normally called from a stopped event."
  (with-current-buffer buf
    (setq-local current-call-stack-id call-stack-id)
    (when-let ((var-tree (dapdbg-ui--get-tree-for-call-stack call-stack-id)))
      (let ((var-map (dapdbg-ui--var-tree-table var-tree)))
        (dolist (id (dapdbg-ui--var-tree-root-ids var-tree))
          (let ((var-obj (gethash id var-map)))
            (puthash :valid nil var-obj)))))))

(defun dapdbg-ui--invalidate-variables-buffers (call-stack-id)
  (dolist (buf-name '(dapdbg-ui--variables-buffer-name dapdbg-ui--registers-buffer-name))
    (let ((buf (dapdbg-ui--get-variables-buffer (symbol-value buf-name))))
      (dapdbg-ui--invalidate-variables-buffer buf call-stack-id))))

(defun dapdbg-ui--handle-scopes-response (call-stack-id parsed-msg)
  (let ((scopes (gethash "scopes" (gethash "body" parsed-msg))))
    (dolist (scope scopes)
      (let ((kind (or (gethash "name" scope) (gethash "presentationHint" scope)))
            (id (gethash "variablesReference" scope)))
        (let ((processor
               (pcase (downcase kind)
                 ("registers" #'dapdbg-ui--registers-update)
                 (_ #'dapdbg-ui--variables-update))))
          (when processor
            (let ((handler (lambda (parsed-msg1)
                             (let ((vars (gethash "variables" (gethash "body" parsed-msg1))))
                               (funcall processor call-stack-id kind vars)))))
              (dapdbg--request-variables id handler))))))))

;; ------------------- disassembly ---------------------

(defconst dapdbg-ui--disassembly-buffer-name "*Disassembly*")

(defun dapdbg-ui--get-disassembly-buffer ()
  (let ((buf-created (dapdbg--get-or-create-buffer dapdbg-ui--disassembly-buffer-name)))
    (when (cdr buf-created) ; freshly-opened
      (with-current-buffer (car buf-created)
        (asm-mode)
        (dapdbg-ui--set-left-margin 2)
        (font-lock-mode -1)
        ;; do this after setting the major mode
        (setq-local
         last-range (cons 0 0))))
    (car buf-created)))

(defun dapdbg-ui--disassembly-render (program-counter instructions)
  (let ((marker-point nil)
        (buf (dapdbg-ui--get-disassembly-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (instruction instructions)
          (let* ((address (gethash "address" instruction))
                 (line (format "%s: %s\n"
                               address
                               (gethash "instruction" instruction))))
            (when (eql (dapdbg--parse-address address) program-counter)
              (setq marker-point (point)))
            (insert (propertize line 'read-only t)))))
      (font-lock-fontify-buffer)
      (when (integer-or-marker-p marker-point)
        (goto-char marker-point)
        (dapdbg-ui-mode--set-instruction-marker buf)))
    (display-buffer buf)))

(defun dapdbg-ui--handle-disassembly (ip-ref parsed-msg)
  (let ((instructions (gethash "instructions" (gethash "body" parsed-msg))))
    (dapdbg-ui--disassembly-render ip-ref instructions)))

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

(defun dapdbg-ui--handle-stacktrace-response (thread-id parsed-msg)
  (let ((stack (gethash "stackFrames" (gethash "body" parsed-msg))))
    (dapdbg-ui--set-call-stack thread-id stack)))

(defun dapdbg-ui--handle-threads-response (parsed-msg)
  nil) ;; TODO

(defun dapdbg-ui--handle-output-event (parsed-msg)
  (let ((body (gethash "body" parsed-msg)))
    (dapdbg-ui--output (gethash "output" body) (gethash "category" body))))

(add-hook 'dapdbg--output-callback-list #'dapdbg-ui--handle-output-event)

(defun dapdbg-ui--handle-stopped-event (_parsed-msg)
  ;; current thread ID has already been set on the session object
  (dapdbg--request-stacktrace nil #'dapdbg-ui--handle-stacktrace-response)
  (dapdbg--request-threads #'dapdbg-ui--handle-threads-response))

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
   `(,dapdbg-ui--disassembly-buffer-name
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)))
  (add-to-list
   'display-buffer-alist
   '((major-mode . dapdbg-ui-call-stack-mode)
     (display-buffer-in-side-window)
     (side . left)
     (slot . 1)))
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
  (add-to-list
   'display-buffer-alist
   '((major-mode . dapdbg-ui-globals-mode)
     (display-buffer-in-side-window)
     (side . right)
     (slot . 3)))
  )

(provide 'dapdbg-ui)
