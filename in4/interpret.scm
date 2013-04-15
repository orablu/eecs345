;;; Wes Rupert - wkr3
;;; EECS 345   - Interpreter 4

(load "environment.scm")
(load "classParser.scm")


;;; Expression abstractions

(define op1 (lambda (expr) (if (null? (cdr   expr)) '() (cadr   expr))))
(define op2 (lambda (expr) (if (null? (cddr  expr)) '() (caddr  expr))))
(define op3 (lambda (expr) (if (null? (cdddr expr)) '() (cadddr expr))))
(define op  (lambda (expr) (if (null? expr)         '() (car    expr))))


;;; Expression evaluation

; Interprets a file and returns the result.
(define interpret (lambda (file)
      (unparse (interpret_funcall '(funcall main) (interpret_global (parser file) (newenv))))
      ))

(define interpret_global (lambda (parsetree env)
    (interpret_statement_list
      parsetree
      env
      (lambda (v) (error "Illegal return"))
      (lambda (v) (error "Illegal break"))
      (lambda (v) (error "Illegal continue"))
      )))

; Returns the value to human-readable format.
(define unparse (lambda (x)
    (cond
      ((and (not (number? x)) x)       'true)
      ((and (not (number? x)) (not x)) 'false)
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
      ((eq? '=        (op stmt)) (interpret_assign  stmt env               ))
      ((eq? 'begin    (op stmt)) (interpret_begin   stmt env ret break cont))
      ((eq? 'if       (op stmt)) (interpret_if      stmt env ret break cont))
      ((eq? 'while    (op stmt)) (interpret_while   stmt env ret           ))
      ((eq? 'break    (op stmt)) (break                  env               ))
      ((eq? 'continue (op stmt)) (cont                   env               ))
      ((eq? 'return   (op stmt)) (ret (interpret_value (op1 stmt) env)     ))
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

; Interprets a function call (e.g. "min(3, 5)").
(define interpret_funcall (lambda (stmt env)
    (call/cc (lambda (ret)
      (interpret_function
        (cadr (lookup (op1 stmt) env))
        (set_formal_params
          (cddr stmt)
          (car (lookup (op1 stmt) env))
          env
          ((caddr (lookup (op1 stmt) env))))
        ret)
      ))))

; Interprets a function definition (e.g. "main() {...}").
(define interpret_fundef (lambda (stmt env)
    (assign
      (op1 stmt)
      (cons (op2 stmt) (cons (op3 stmt) (cons (lambda () env) '())))
      (declare (op1 stmt) env)
      )))

(define interpret_function (lambda (stmt env ret)
    (popframe
      (interpret_statement_list
        stmt
        (pushframe env)
        ret
        (lambda (v) (error "Illegal break"))
        (lambda (v) (error "Illegal continue")))
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
      ((eq? 'function (op stmt)) (interpret_fundef  stmt env               ))
      ((eq? 'funcall  (op stmt)) (interpret_funcall stmt env               ))
      ((eq? 'var      (op stmt)) (interpret_declare stmt env               ))
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
      (else  (error "Symbol not recognized:" (op stmt) '-> stmt))
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

; Sets the formal parameters of the environment to the values in the given parameters.
(define set_formal_params (lambda (params formals env funcenv)
    (cond
      ((and (null? params) (null? formals)) env)
      ((or  (null? params) (null? formals)) (error "Invalid number of arguments"))
      ((or (null? (cdr params)) (null? (cdr formals))) (assign
        (car formals)
        (interpret_value (car params) env)
        (declare (car formals) funcenv)))
      (else (assign
        (car formals)
        (interpret_value (car params) env)
        (declare
          (car formals)
          (set_formal_params
            (cdr params)
            (cdr formals)
            env
            funcenv))))
      )))

