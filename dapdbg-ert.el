;; Unit tests for dapdbg
;;
;; run this file with:
;; emacs -batch -L . -l ert -l dapdbg-ert.el -f ert-run-tests-batch-and-exit
;;
;; add this as a pre-commit hook:
;;
;; tests=`find . -name '*-ert.el' -printf ' -l %p'`
;; if ! emacs -batch -L . -l ert $tests -f ert-run-tests-batch-and-exit ; then
;;     exit 1
;; fi

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
   (let ((msg "Content-Length: 77\r
\r
{\"command\":\"launch\",\"request_seq\":3,\"seq\":0,\"success\":true,\"type\":\"response\"}Content-Length: 187\r
\r
{\"body\":{\"isLocalProcess\":true,\"name\":\"/home/stu/dev/advent_of_code/2022/rust/target/debug/day10\",\"startMethod\":\"launch\",\"systemProcessId\":34279},\"event\":\"process\",\"seq\":0,\"type\":\"event\"}Content-Length: 47\r
\r
{\"event\":\"xinitialized\",\"seq\":0,\"type\":\"event\"}"))
     (should (equal '(99 210 69) (dapdbg--handle-server-message nil msg))))))

(ert-deftest can-route-success-callback ()
  (with-test-setup
   (let* ((cb-value nil)
          (seq-number 345)
          (test-message (list :foo "bar" :success t :type "response" :request_seq seq-number)))
     (let ((h-b (dapdbg--base-protocol test-message nil)))
       (dapdbg--register-callback
        seq-number (lambda (msg)
                     (setq cb-value (gethash "foo" msg))))
       (dapdbg--handle-server-message nil (format "%s\r\n%s" (car h-b) (cdr h-b)))
       (should (string= "bar" cb-value))
       ))))

(defmacro with-processor (advice-fn form)
  `(progn
     (advice-add #'process-send-string :override ,advice-fn)
     ,form
     (advice-remove #'process-send-string ,advice-fn)))

(defun echo-processor (_ req-str)
  "Simple processor which always returns success and nothing else."
  (pcase-let ((`(:parsed-length ,length :parsed-msg ,parsed-msg)
               (dapdbg--parse-message req-str)))
    (let* ((req-seq (gethash "seq" parsed-msg))
           (response (list :request_seq req-seq :type "response" :success t))
           (h-b (dapdbg--base-protocol response 0)))
      (dapdbg--handle-server-message nil (format "%s\r\n%s" (car h-b)  (cdr h-b))))))

(ert-deftest can-chain-requests ()
  (with-test-setup
   (with-processor
    #'echo-processor
    (let ((responses nil))
      (dapdbg--chain-requests
       (list
        (list :command "foo" :args nil
              :callback (lambda (_parsed-msg) (setq responses (cons "foo" responses))))
        (list :command "bar" :args nil
              :callback (lambda (_parsed-msg) (setq responses (cons "bar" responses))))
        ))
      (should (equal responses '("bar" "foo")))))))
