; Load the project first!
(load "interpret.scm")

; Verifies the test with the expected result.
(define verify
  (lambda (file result)
    (if (eq? (interpret file) result)
      (printf "~a    pass~%" file)
      (printf "~a    FAIL~%    Expected result:    ~a~%    Computed result:    ~a~%" file result (interpret file))
      )))

; Tests are added in this format
; (verify "tests\\NAME.js" 'RESULT)

; Tests for behavior from the first interpreter.
(verify "tests\\01.js" 150)
(verify "tests\\02.js" -4)
(verify "tests\\03.js" 10)
(verify "tests\\04.js" 16)
(verify "tests\\05.js" 220)
(verify "tests\\06.js" 5)
(verify "tests\\07.js" 6)
(verify "tests\\08.js" 10)
(verify "tests\\09.js" 5)
(verify "tests\\10.js" -39)
(verify "tests\\15.js" "true")
(verify "tests\\16.js" 100)
(verify "tests\\17.js" "false")
(verify "tests\\18.js" "true")

; Tests for new behavior.
(verify "tests\\11.js" 30)
(verify "tests\\12.js" -102312)
(verify "tests\\13.js" 15)
(verify "tests\\14.js" 1)
(verify "tests\\19.js" 2)
(verify "tests\\20.js" 3)
(verify "tests\\21.js" 20)
