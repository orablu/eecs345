; Load the project first!
(load "interpret.scm")

; Verifies the test with the expected result.
(define verify
  (lambda (test result)
    (if (eq? test result)
      (printf "Test Passed.~%")
      (printf "Test failed. Computed result:~%~a~%" test)
      )))

; Tests. It's that simple.
(verify (interpret "tests\\1.js") 30)
(verify (interpret "tests\\2.js") 30)
