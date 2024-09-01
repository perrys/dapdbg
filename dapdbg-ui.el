(require 'dapdbg)
(require 'gdb-mi)

(make-variable-buffer-local 'dapdbg-ui--buffer-breakpoints)

(defun dapdbg-ui--set-source-breakpoint (filename line enabled bp-number)
  (save-excursion
    (with-current-buffer (find-file filename)
      (gdb-put-breakpoint-icon enabled bp-number line))))





