;;; Wes Rupert - wkr3
;;; EECS 345   - Project 01

(load "verySimpleParser.scm")


;;; Expression abstractions

(define op1 (lambda (expr) (if (null? (cdr expr)) '() (car (cdr expr)))))
(define op2 (lambda (expr) (if (null? (cdr (cdr expr))) '() (car (cdr (cdr expr))))))
(define op3 (lambda (expr) (if (null? (cdr (cdr (cdr expr)))) '() (car (cdr (cdr (cdr expr)))))))
(define operator (lambda (expr) (if (null? expr) '() (car expr))))
(define opcount (lambda (expr) (if (null? expr) (0) (+ 1 (opcount (cdr expr))))))


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
(define interpret_assign (lambda stmt env
    (if (declared? (op1 stmt))
      (put (op1 stmt) (op2 stmt) (drop (op1 stmt) env))
      (env)
      )))

; Interprets a declaration (e.g. "var x;" or "var y = 10").
(define interpret_declare (lambda stmt env
    (put (op1 stmt) (interpret_value (op2 stmt)) env)))

; Interprets an if statement, with optional else.
(define interpret_if (lambda stmt env
    (cond
      ((interpret_condition (op1 stmt)) (interpret_statement_list (op2 stmt) env))
      ((null? (op3 stmt)) env)
      (else (interpret_statement_list (op3 stmt) env))
      )))

; Interprets a return statement.
(define interpret_return (lambda (stmt env)
    (if (declared? return)
      (env)
      (put 'return (interpret_value (op1 stmt) env) env)
      )))

; Interprets the value of a mathematical statement.
(define interpret_value (lambda (stmt env)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((not (pair? stmt)) (lookup stmt env))
      ((null? (cdr stmt)) (interpret_value (car stmt) env))
      ((eq? '+ (operator stmt)) (+         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '- (operator stmt)) (-         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '* (operator stmt)) (*         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '/ (operator stmt)) (quotient  (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '% (operator stmt)) (remainder (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      (else '())
      )))

; Interprets the value of a logical statement.
(define interpret_condition (lambda (stmt env)
    (cond
      ((boolean? stmt) stmt)
      ((not (pair? stmt)) (lookup (stmt env)))
      ((null? (cdr stmt)) (interpret_condition (car stmt) env))
      ((eq? '== (operator stmt)) (=      (interpret_condition (op1 stmt) env) (interpret_condition (op2 stmt) env)))
      ((eq? '!= (operator stmt)) (not (= (interpret_condition (op1 stmt) env) (interpret_condition (op2 stmt) env))))
      ((eq? '<  (operator stmt)) (<      (interpret_condition (op1 stmt) env) (interpret_condition (op2 stmt) env)))
      ((eq? '>  (operator stmt)) (>      (interpret_condition (op1 stmt) env) (interpret_condition (op2 stmt) env)))
      ((eq? '<= (operator stmt)) (<=     (interpret_condition (op1 stmt) env) (interpret_condition (op2 stmt) env)))
      ((eq? '>= (operator stmt)) (>=     (interpret_condition (op1 stmt) env) (interpret_condition (op2 stmt) env)))
      (else '())
      )))


;;; ENVIRONMENT LOGIC

; Generates a new environment.
(define newenv (lambda () '()))

; Sets the value of the named variable in the environment to val.
(define put (lambda (name val env) (cons (cons name val) (drop name env))))

; Removes a variable from the environment.
(define drop (lambda (name env)
    (cond
      ((null? env) '())
      ((not (pair? (car env))) (drop name (cdr env)))
      ((eq? name (car env)) (drop name (cdr env)))
      (else (cons (car env) (drop name (cdr env))))
      )))

; Tests whether the variable is delcared.
(define declared? (lambda (x env)
    (cond
      ((null? env) #f)
      ((and (pair? (car env)) (eq? x (car env))) #t)
      (else (declared? x (cdr env)))
      )))

; Returns the value of the given variable name.
(define lookup (lambda (x env)
    (cond
      ((null? env) '())
      ((not (pair? (car env))) '())
      ((eq? x (car (car env))) (cdr (car env)))
      (else (lookup x (cdr env)))
      )))

