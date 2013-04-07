;;; Wes Rupert - wkr3
;;; EECS 345   - Interpreter 3

(load "environment.scm")
(load "functionParser.scm")
;(load "loopSimpleParser.scm")


;;; Expression abstractions
(define op1 (lambda (expr) (if (null? (cdr   expr)) '() (cadr   expr))))
(define op2 (lambda (expr) (if (null? (cddr  expr)) '() (caddr  expr))))
(define op3 (lambda (expr) (if (null? (cdddr expr)) '() (cadddr expr))))
(define op  (lambda (expr) (if (null? expr)         '() (car    expr))))


;;; Expression evaluation

; Interprets a file and returns the result.
(define interpret (lambda (file)
    (unparse (call/cc (lambda (ret)
      (interpret_statement_list
        (parser file)
        (newenv)
        ret
        (lambda (v) (error "Illegal break"))
        (lambda (v) (error "Illegal continue"))
        ))))))

; Returns the value to human-readable format.
(define unparse (lambda (x)
    (cond
      ((and (not (number? x)) x)       "true" )
      ((and (not (number? x)) (not x)) "false")
      (else x)
      )))

; Interprets a list of parsed statements.
(define interpret_statement_list (lambda (parsetree env ret break cont)
    (if (null? parsetree)
     env
     (interpret_statement_list (cdr parsetree) (interpret_statement (car parsetree) env ret break cont) ret break cont)
     )))

; Interprets a single statement.
(define interpret_statement (lambda (stmt env ret break cont)
    (cond
      ((eq? 'return   (op stmt)) (ret (interpret_value (op1 stmt) env)     ))
      ((eq? '=        (op stmt)) (interpret_assign  stmt env               ))
      ((eq? 'begin    (op stmt)) (interpret_begin   stmt env ret break cont))
      ((eq? 'var      (op stmt)) (interpret_declare stmt env               ))
      ((eq? 'if       (op stmt)) (interpret_if      stmt env ret break cont))
      ((eq? 'while    (op stmt)) (interpret_while   stmt env ret           ))
      ((eq? 'break    (op stmt)) (break                  env               ))
      ((eq? 'continue (op stmt)) (cont                   env               ))
      (else                      (interpret_value   stmt env               ))
      )))

; Interprets an assignment (e.g. "x = 10;").
(define interpret_assign (lambda (stmt env)
    (assign (op1 stmt) (interpret_value (op2 stmt) env) env)
    ))

; Interprets a block (e.g. "{...}").
(define interpret_begin (lambda (stmt env ret break cont)
    (popframe
      (interpret_statement_list
        (cdr stmt)
        (pushframe env)
        ret
        (lambda (v)
          (break (popframe v)))
        (lambda (v)
          (cont (popframe v)))
        ))))

; Interprets a declaration (e.g. "var x;" or "var y = 10").
(define interpret_declare (lambda (stmt env)
    (if (null? (op2 stmt))
      (declare (op1 stmt) env)
      (assign (op1 stmt) (interpret_value (op2 stmt) env) (declare (op1 stmt) env))
      )))

; Interprets an if statement (e.g. "if (...) ...;" or "if (...) {...} else {...}").
(define interpret_if (lambda (stmt env ret break cont)
    (cond
      ((interpret_value (op1 stmt) env) (interpret_statement (op2 stmt) env ret break cont))
      ((null? (op3 stmt)) env)
      (else (interpret_statement (op3 stmt) env ret break cont))
      )))

; Interprets the value of a mathematical statement.
(define interpret_value (lambda (stmt env)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((eq? 'true  stmt) #t)
      ((eq? 'false stmt) #f)
      ((not (list? stmt)) (lookup stmt env))
      ((null? (cdr stmt)) (interpret_value (car stmt) env))
      ((and (eq? '- (op stmt)) (null? (op2 stmt))) (-                  (interpret_value (op1 stmt) env)))
      ((eq? '+  (op stmt)) (+         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '-  (op stmt)) (-         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '*  (op stmt)) (*         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '/  (op stmt)) (quotient  (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '%  (op stmt)) (remainder (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '== (op stmt)) (eq?       (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '!= (op stmt)) (not (=    (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env))))
      ((eq? '<  (op stmt)) (<         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '>  (op stmt)) (>         (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '<= (op stmt)) (<=        (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '>= (op stmt)) (>=        (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '&& (op stmt)) (and       (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '|| (op stmt)) (or        (interpret_value (op1 stmt) env) (interpret_value (op2 stmt) env)))
      ((eq? '!  (op stmt)) (not       (interpret_value (op1 stmt) env)                                 ))
      (else  (error (cons "Symbol not recognized" (op stmt))))
      )))

; Interprets the value of a while loop.
(define interpret_while (lambda (stmt env ret)
    (call/cc (lambda (break)
      (letrec ((loop (lambda (test body env)
          (if (interpret_value test env)
            (loop test body (call/cc (lambda (cont) (interpret_statement body env ret break cont))))
            env))))
          (loop (op1 stmt) (op2 stmt) env)))
        )))

