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

(ert-deftest can-parse-compound-msg ()
  (dapdbg--enrich-process-symbol)
  (let ((msg "Content-Length: 77

{\"command\":\"launch\",\"request_seq\":3,\"seq\":0,\"success\":true,\"type\":\"response\"}Content-Length: 187

{\"body\":{\"isLocalProcess\":true,\"name\":\"/home/stu/dev/advent_of_code/2022/rust/target/debug/day10\",\"startMethod\":\"launch\",\"systemProcessId\":34279},\"event\":\"process\",\"seq\":0,\"type\":\"event\"}Content-Length: 46

{\"event\":\"initialized\",\"seq\":0,\"type\":\"event\"}"))
    (should (equal '(99 210 68) (dapdbg--filter nil msg)))))

(ert-deftest can-route-callback ()
  (let* ((cb-value nil)
         (seq-number 345)
         (test-message (list :foo "bar" :type "response" :request_seq seq-number)))
    (pcase-let ((`(:header ,hdrs :body ,body :seq ,seq) (dapdbg--base-protocol test-message nil)))
      (dapdbg--enrich-process-symbol)
      (dapdbg--register-callback
       seq-number (lambda (msg)
                    (setq cb-value (gethash "foo" msg))))
      (dapdbg--filter nil (format "%s\r\n%s" hdrs body))
      (should (string= "bar" cb-value))
      )))
