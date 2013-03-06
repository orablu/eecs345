; Function to check that a given test was successful
(define verify
  (lambda (test result)
    (if (eq? test result)
      (printf "Test Passed.~%")
      (printf "Test failed. Computed result:~%~a~%" test)
      )))

; TODO: Get verify-list to evaluate expressions inside lists.
; (define verify (lambda (tests) (verify-list tests 1 0)))
(define test (lambda (tests) (car (car tests))))
(define result (lambda (tests) (car (cdr (car tests)))))
(define verify-list
  (lambda (tests i f)
    (cond
      ((null? tests) (printf "Tests complete. ~a passed, ~a failed.~%" (- (- i f) 1) f))
      ((eq? (test tests) (result tests)) (verify-list (cdr tests) (+ i 1) f))
      (else (begin
              (printf "Test ~a failed, returned:~%~a~%" i (test tests))
              (verify-list (cdr tests) (+ i 1) (+ f 1))
              ))
      )))
