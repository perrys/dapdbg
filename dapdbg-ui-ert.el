;; Unit tests for dapdbg-ui
;;
;; run this file with:
;; emacs -batch -L . -l ert -l dapdbg-ui-ert.el -f ert-run-tests-batch-and-exit

(require 'dapdbg-ui)

(defun dapdbg-ui--test-tree ()
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

(defun dapdbg-ui--test-object (alist)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry alist)
      (puthash (car entry) (cdr entry) table))
    table))

(ert-deftest can-create-tree ()
  (let* ((mytree (dapdbg-ui--test-tree))
         (mytable (dapdbg-ui--var-tree-table mytree))
         (root-ids (dapdbg-ui--var-tree-root-ids mytree)))
    (should (eq 3 (hash-table-count mytable)))
    (should (eq 1 (length root-ids)))
    (let ((child-ids (reverse (gethash :child-ids (gethash (car root-ids) mytable)))))
      (should (equal "foo" (gethash "value" (dapdbg-ui--get-var (car child-ids) mytree))))
      (should (equal "bar" (gethash "value" (dapdbg-ui--get-var (cadr child-ids) mytree)))))))

(ert-deftest can-serialize-tree ()
  (let ((mytree (dapdbg-ui--test-tree)))
    (let ((result (dapdbg-ui--make-variables-tabulated-list mytree)))
      (should result)
      (should (eq 3 (length result)))
      (should (equal "Locals/args" (car (nth 0 result))))
      (should (equal "Locals/args/[0]" (car (nth 1 result))))
      (should (equal "Locals/args/[1]" (car (nth 2 result)))))))

(ert-deftest can-update-tree ()
  (let* ((mytree (dapdbg-ui--test-tree))
         (mytable (dapdbg-ui--var-tree-table mytree))
         (updates (dapdbg-ui--add-to-variables-tree
                   "Locals" (list (copy-hash-table (gethash "Locals/args" mytable))) mytree)))
    (should (eq 1 (length updates)))
    (should (equal (cons "Locals/args"  3) (car updates)))))

(ert-deftest can-update-instruction-cache ()
  (let* ((instruction-cache (make-hash-table :test 'eql))
         (instructions (list
                        (dapdbg-ui--test-object '(("address" . "0xfa") ("foo" . "bar")))
                        (dapdbg-ui--test-object '(("address" . "0xff")))
                        (dapdbg-ui--test-object '(("address" . "0x101") ("foo" . "baz"))))))
    (dapdbg-ui--update-instruction-cache instructions instruction-cache)
    (should (equal '(#xff #x101 nil)
                   (mapcar (lambda (e) (plist-get e :next))
                           (dapdbg-ui--mapchain instruction-cache #xfa :next))))
    (should (equal '(#xff #xfa nil)
                   (mapcar (lambda (e) (plist-get e :prev))
                           (dapdbg-ui--mapchain instruction-cache #x101 :prev))))
    (should (equal "bar" (gethash "foo" (plist-get (gethash #xfa instruction-cache) :data))))
    (should (not (plist-get (gethash #xfa instruction-cache) :prev)))
    (should (eql #xff (plist-get (gethash #xfa instruction-cache) :next)))
    (should (equal "baz" (gethash "foo" (plist-get (gethash #x101 instruction-cache) :data))))
    (should (not (plist-get (gethash #x101 instruction-cache) :next)))
    (should (eql #xff (plist-get (gethash #x101 instruction-cache) :prev)))
    ))

(ert-deftest can-update-join-lists ()
  (let* ((instruction-cache (make-hash-table :test 'eql))
         (ilist1 (list
                  (dapdbg-ui--test-object '(("address" . "0xfa") ("foo" . "v1")))
                  (dapdbg-ui--test-object '(("address" . "0xff") ("foo" . "v2")))
                  (dapdbg-ui--test-object '(("address" . "0x101") ("foo" . "v3")))))
         (ilist2 (list
                  (dapdbg-ui--test-object '(("address" . "0xff") ("foo" . "v4")))
                  (dapdbg-ui--test-object '(("address" . "0x101") ("foo" . "v5")))
                  (dapdbg-ui--test-object '(("address" . "0x102") ("foo" . "v6"))))))
    ;; join to start
    (dapdbg-ui--update-instruction-cache ilist2 instruction-cache)
    (dapdbg-ui--update-instruction-cache ilist1 instruction-cache)
    (should (equal '(#xff #x101 #x102 nil)
                   (mapcar (lambda (e) (plist-get e :next))
                           (dapdbg-ui--mapchain instruction-cache #xfa :next))))
    (should (equal '(#x101 #xff #xfa nil)
                   (mapcar (lambda (e) (plist-get e :prev))
                           (dapdbg-ui--mapchain instruction-cache #x102 :prev))))
    (should (equal '("v1" "v4" "v5" "v6")
                   (mapcar (lambda (e) (gethash "foo" (plist-get e :data)))
                           (dapdbg-ui--mapchain instruction-cache #xfa :next))))
    ;; join to end
    (clrhash instruction-cache)
    (dapdbg-ui--update-instruction-cache ilist1 instruction-cache)
    (dapdbg-ui--update-instruction-cache ilist2 instruction-cache)
    (should (equal '(#xff #x101 #x102 nil)
                   (mapcar (lambda (e) (plist-get e :next))
                           (dapdbg-ui--mapchain instruction-cache #xfa :next))))
    (should (equal '(#x101 #xff #xfa nil)
                   (mapcar (lambda (e) (plist-get e :prev))
                           (dapdbg-ui--mapchain instruction-cache #x102 :prev))))
    (should (equal '("v1" "v2" "v3" "v6")
                   (mapcar (lambda (e) (gethash "foo" (plist-get e :data)))
                           (dapdbg-ui--mapchain instruction-cache #xfa :next))))
    ))
