;;; Wes Rupert - wkr3
;;; EECS 345   - Project 01

(load "verySimpleParser.scm")


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
      (put (op1 stmt) (op2 stmt) (drop (op1 stmt) env))
      env
      )))

; Interprets a declaration (e.g. "var x;" or "var y = 10").
(define interpret_declare (lambda (stmt env)
    (put (op1 stmt) (interpret_value (op2 stmt) env) env)))

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
      (put 'return (interpret_value (op1 stmt) env) env)
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

; The environment is stored as a list of (value . name) pairs. It is stored
; backwards like because the name will always be a string. Storing it as
; (value . name) allows value to be the empty list ('()) and not require any
; additional complex logic to properly create a pair of (name . '()).

; Generates a new environment.
(define newenv (lambda () '()))

; Expression abstractions. 
(define getname (lambda (var) (cdr var)))
(define getval (lambda (var) (car var)))

; Sets the value of the named variable in the environment to val.
(define put (lambda (name val env) (cons (cons val name) (drop name env))))

; Removes a variable from the environment.
(define drop (lambda (name env)
    (cond
      ((null? env) '())
      ((not (pair? (car env))) (drop name (cdr env)))
      ((eq? name (getname (car env))) (drop name (cdr env)))
      (else (cons (car env) (drop name (cdr env))))
      )))

; Tests whether the variable is delcared.
(define declared? (lambda (x env)
    (cond
      ((null? env) #f)
      ((not (pair? (car env))) (declared? x (cdr env)))
      ((eq? x (getname (car env))) #t)
      (else (declared? x (cdr env)))
      )))

; Returns the value of the given variable name.
(define lookup (lambda (x env)
    (cond
      ((null? env) '())
      ((not (pair? (car env))) '())
      ((eq? x (getname (car env))) (getval (car env)))
      (else (lookup x (cdr env)))
      )))

