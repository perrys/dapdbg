;; Unit tests for dapdbg-ui
;;
;; run this file with:
;; emacs -batch -L . -l ert -l dapdbg-ui-ert.el -f ert-run-tests-batch-and-exit

(require 'dapdbg-ui)

(defun dapdbg-ui-test-create-tree ()
  (let ((counter 0)
        (root (list))
        (var-map (make-hash-table :test 'equal))
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
    (dapdbg-ui--add-to-variables-tree (car msgs) nil 'root 'counter var-map)
    (dapdbg-ui--add-to-variables-tree (cadr msgs) 1 'root 'counter var-map)
    (list :root root :counter counter :map var-map)))


(ert-deftest can-create-tree ()
  (pcase-let ((`(:root ,root :counter ,counter :map ,var-map) (dapdbg-ui-test-create-tree)))
    (should (eq 3 (hash-table-count var-map)))
    (should (eq 1 (length root)))
    (let ((children (reverse (gethash :children (car root)))))
      (should (equal "foo" (gethash "value" (car children))))
      (should (equal "bar" (gethash "value" (cadr children)))))
    ))

(ert-deftest can-serialize-tree ()
  (pcase-let ((`(:root ,root :counter ,counter :map ,var-map) (dapdbg-ui-test-create-tree)))
  (let ((result (dapdbg-ui--make-tablulated-list-entries root)))
    (message "result1 = %s" result))))  ;
