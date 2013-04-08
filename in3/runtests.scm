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
    (verify "tests\\1\\01.js" 150)
    (verify "tests\\1\\02.js" -4)
    (verify "tests\\1\\03.js" 10)
    (verify "tests\\1\\04.js" 16)
    (verify "tests\\1\\05.js" 220)
    (verify "tests\\1\\06.js" 5)
    (verify "tests\\1\\07.js" 6)
    (verify "tests\\1\\08.js" 10)
    (verify "tests\\1\\09.js" 5)
    (verify "tests\\1\\10.js" -39)
    (verify "tests\\1\\15.js" 'true)
    (verify "tests\\1\\16.js" 100)
    (verify "tests\\1\\17.js" 'false)
    (verify "tests\\1\\18.js" 'true)

; Tests for behavior from the first interpreter.
    (verify "tests\\2\\01.js" 100)
    (verify "tests\\2\\02.js" 20)
    (verify "tests\\2\\03.js" 6)
    (verify "tests\\2\\04.js" -1)
    (verify "tests\\2\\05.js" 789)
    (verify "tests\\2\\06.js" 2)
    (verify "tests\\2\\07.js" 164)
    ;(verify "tests\\2\\08.js" 'error)
    ;(verify "tests\\2\\09.js" 'error)
    ;(verify "tests\\2\\10.js" 'error)
    (verify "tests\\2\\12.js" 12)
    (verify "tests\\2\\13.js" 32)
