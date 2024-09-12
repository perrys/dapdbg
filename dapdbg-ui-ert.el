;; Unit tests for dapdbg-ui
;;
;; run this file with:
;; emacs -batch -L . -l ert -l dapdbg-ui-ert.el -f ert-run-tests-batch-and-exit

(require 'dapdbg-ui)

(defun dapdbg-ui-test-create-tree ()
  (let ((tree (make-dapdbg-ui--var-tree))
        (msgs (mapcar (lambda (msg) (gethash "variables" (dapdbg--parse-json msg))) '("
{
    \"variables\": [
      {
        \"variablesReference\": 3,
        \"name\": \"args\",
        \"value\": \"Vec(size=2)\",
        \"indexedVariables\": 2,
        \"type\": \"alloc::vec::Vec<alloc::string::String, alloc::alloc::Global>\"
      }
    ]
}
" "
{
   \"variables\": [
      {
        \"variablesReference\": 0,
        \"name\": \"[0]\",
        \"value\": \"foo\",
        \"type\": \"alloc::string::String\"
      },
      {
        \"variablesReference\": 0,
        \"name\": \"[1]\",
        \"value\": \"bar\",
        \"type\": \"alloc::string::String\"
      }
    ]
}
"))))
    (dapdbg-ui--add-to-variables-tree "Locals" (car msgs) tree)
    (dapdbg-ui--add-to-variables-tree "Locals/args" (cadr msgs) tree)
    tree))


(ert-deftest can-create-tree ()
  (let* ((mytree (dapdbg-ui-test-create-tree))
         (mytable (dapdbg-ui--var-tree-table mytree))
         (root-ids (dapdbg-ui--var-tree-root-ids mytree)))
    (should (eq 3 (hash-table-count mytable)))
    (should (eq 1 (length root-ids)))
    (let ((child-ids (reverse (gethash :child-ids (gethash (car root-ids) mytable)))))
      (should (equal "foo" (gethash "value" (dapdbg-ui--get-var (car child-ids) mytree))))
      (should (equal "bar" (gethash "value" (dapdbg-ui--get-var (cadr child-ids) mytree)))))))

(ert-deftest can-serialize-tree ()
  (let ((mytree (dapdbg-ui-test-create-tree)))
    (let ((result (dapdbg-ui--make-variables-list mytree)))
      (should result)
      (should (eq 3 (length result)))
      (should (equal "Locals/args" (car (nth 0 result))))
      (should (equal "Locals/args/[0]" (car (nth 1 result))))
      (should (equal "Locals/args/[1]" (car (nth 2 result)))))))

(ert-deftest can-update-tree ()
  (let* ((mytree (dapdbg-ui-test-create-tree))
         (mytable (dapdbg-ui--var-tree-table mytree))
         (updates (dapdbg-ui--add-to-variables-tree
                   "Locals" (list (copy-hash-table (gethash "Locals/args" mytable))) mytree)))
    (should (eq 1 (length updates)))
    (should (equal (cons "Locals/args"  3) (car updates)))))

    

