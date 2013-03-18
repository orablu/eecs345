;;; Wes Rupert - wkr3
;;; EECS 345   - Interpreter 2

(load "loopSimpleParser.scm")


;;; Expression abstractions

(define op1 (lambda (expr) (if (null? (cdr expr)) '() (car (cdr expr)))))
(define op2 (lambda (expr) (if (null? (cdr (cdr expr))) '() (car (cdr (cdr expr))))))
(define op3 (lambda (expr) (if (null? (cdr (cdr (cdr expr)))) '() (car (cdr (cdr (cdr expr)))))))
(define operator (lambda (expr) (if (null? expr) '() (car expr))))


;;; Expression evaluation

; Interprets a file and returns the result.
(define interpret (lambda (file)
    (lookup 'return (interpret_statement_list (parser file) (newenv)))))

; Interprets a list of parsed statements.
(define interpret_statement_list (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret_statement_list (cdr parsetree) (interpret_statement (car parsetree) env)))
     )))

; Interprets a single statement.
(define interpret_statement (lambda (stmt env)
    (cond
      ((eq? '=      (operator stmt)) (interpret_assign  stmt env))
      ((eq? 'var    (operator stmt)) (interpret_declare stmt env))
      ((eq? 'if     (operator stmt)) (interpret_if      stmt env))
      ((eq? 'return (operator stmt)) (interpret_return  stmt env))
      (else                          (interpret_value   stmt env))
      )))

; Interprets an assignment (e.g. "x = 10;").
(define interpret_assign (lambda (stmt env)
    (if (declared? (op1 stmt) env)
      (assign (op1 stmt) (op2 stmt) (drop (op1 stmt) env))
      env
      )))

; Interprets a declaration (e.g. "var x;" or "var y = 10").
(define interpret_declare (lambda (stmt env)
    (declare (op1 stmt) (interpret_value (op2 stmt) env) env)))

; Interprets an if statement.
(define interpret_if (lambda (stmt env)
    (cond
      ((interpret_value (op1 stmt) env) (interpret_statement (op2 stmt) env))
      ((null? (op3 stmt)) env)
      (else (interpret_statement (op3 stmt) env))
      )))

; Interprets a return statement.
(define interpret_return (lambda (stmt env)
    (if (declared? 'return env)
      env
      (assign 'return (interpret_value (op1 stmt) env) (declare 'return env))
      )))

; Interprets the value of a mathematical statement.
(define interpret_value (lambda (stmt env)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((not (list? stmt)) (lookup stmt env))
      ((null? (cdr stmt)) (interpret_value (car stmt) env))
      ((eq? '+  (operator stmt)) (+         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '-  (operator stmt)) (-         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '*  (operator stmt)) (*         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '/  (operator stmt)) (quotient  (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '%  (operator stmt)) (remainder (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '== (operator stmt)) (=         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '!= (operator stmt)) (not (=    (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env))))
      ((eq? '<  (operator stmt)) (<         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '>  (operator stmt)) (>         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '<= (operator stmt)) (<=        (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '>= (operator stmt)) (>=        (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      (else '())
      )))


;;; ENVIRONMENT LOGIC

; The environment is stored as a list of frames. Each frame has two lists of
; equal size, the first of variable names, and the second of their values.

; Generates a new environment. 
(define newenv (lambda () '((()()))))

; Gets the item at index i, starting from 0.
(define itemat (lambda (i l)
    (if (= i 0)
      (car l)
      (itemat (- i 1) (cdr l))
      )))

; Removes the item at index i, starting from 0.
(define removeat (lambda (i l)
    (if (= i 0)
      (cdr l)
      (cons (car l) (removeat (- i 1) (cdr l)))
      )))

(define replaceat (lambda (x i l)
    (if (= i 0)
      (cons x (cdr l))
      (cons (car l) (replaceat x (- i 1) (cdr l)))
      )))

; Gets the index of the given item.
(define getindex (lambda (x l)
    (letrec
      ((getindex-cps (lambda (x l k)
        (cond
          ((null? l) -1)
          ((eq? x (car l)) (k 0))
          (else (getindex-cps x (cdr l) (lambda (v) (k (+ i 1)))))
          ))))
      (getindex-cps x l (lambda (v) v))
    )))

; Frame abstractions.
(define names (lambda (frame) (car frame)))
(define vals (lambda (frame) (car (cdr frame))))
(define inframe? (lambda (x frame) (not (= -1 (getindex x (names frame))))))
(define pushframe (lambda (env) (cons '(()()) env)))
(define popframe (lambda (env) (cdr env)))
(define getval (lambda (x frame) (itemat (getindex x (names frame)) (vals frame))))

; Declares a new variable in the environment.
(define declare (lambda (name env)
	(cons
	  (cons (cons name (names (car env)))
	        (cons (cons 0 (vals (car env)))
	               '()))
      (cdr env)
      )))

; Tests whether the variable is delcared.
(define declared? (lambda (x env)
    (if (inframe? (car env))
      #t
      (declared? (cdr env))
      )))

; Sets the value of the named variable in the environment to val.
(define assign (lambda (name val env)
    (if (inframe? name (car env))
	  (cons
	    (cons (names (car env))
	          (cons (replaceat val (getindex name (names (car env))) (vals (car env)))
	                 '()))
        (cdr env))
      (cons (car env) (assign name val (cdr env)))
      )))

; Returns the value of the given variable name.
(define lookup (lambda (x env)
    (if (inframe? (car env))
      (getval x (car env))
      (lookup x (cdr env))
      )))

