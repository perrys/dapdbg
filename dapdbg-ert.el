;; Unit tests for dapdbg
;;
;; run this file with:
;; emacs -batch -L . -l ert -l dapdbg-ert.el -f ert-run-tests-batch-and-exit

(require 'dapdbg)

(ert-deftest can-parse-header ()
  (let ((hdr "Content-Length: 301
Accept: application/json
"))
    (should (= 301 (dapdbg--parse-header hdr)))
    ))

(defmacro with-test-setup (form)
  (list 'progn
        '(setq dapdbg--ssn (make-dapdbg-session))
        form
        '(setq dapdbg--ssn nil)))

(ert-deftest can-parse-compound-msg ()
  (with-test-setup
   (let ((msg "Content-Length: 77

{\"command\":\"launch\",\"request_seq\":3,\"seq\":0,\"success\":true,\"type\":\"response\"}Content-Length: 187

{\"body\":{\"isLocalProcess\":true,\"name\":\"/home/stu/dev/advent_of_code/2022/rust/target/debug/day10\",\"startMethod\":\"launch\",\"systemProcessId\":34279},\"event\":\"process\",\"seq\":0,\"type\":\"event\"}Content-Length: 47

{\"event\":\"xinitialized\",\"seq\":0,\"type\":\"event\"}"))
     (should (equal '(99 210 69) (dapdbg--handle-server-message nil msg))))))

(ert-deftest can-route-success-callback ()
  (with-test-setup
   (let* ((cb-value nil)
          (seq-number 345)
          (test-message (list :foo "bar" :type "response" :success t :request_seq seq-number)))
     (pcase-let ((`(:header ,hdrs :body ,body :seq ,seq) (dapdbg--base-protocol test-message nil)))
       (dapdbg--register-callback
        seq-number (lambda (msg)
                     (message "called back: %s" msg)
                     (setq cb-value (gethash "foo" msg))))
       (dapdbg--handle-server-message nil (format "%s\r\n%s" hdrs body))
       (should (string= "bar" cb-value))
       ))))
