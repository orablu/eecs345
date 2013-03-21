;;; Wes Rupert - wkr3
;;; EECS 345   - Interpreter 2

(load "loopSimpleParser.scm")


;;; Expression abstractions
(define op1 (lambda (expr) (if (null? (cdr   expr)) '() (cadr   expr))))
(define op2 (lambda (expr) (if (null? (cddr  expr)) '() (caddr  expr))))
(define op3 (lambda (expr) (if (null? (cdddr expr)) '() (cadddr expr))))
(define op  (lambda (expr) (if (null? expr)         '() (car    expr))))


;;; Expression evaluation

; Interprets a file and returns the result.
(define interpret (lambda (file)
    (unparse (call/cc (lambda (ret) (interpret_statement_list (parser file) (newenv) ret ret ret))))))

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
    (popframe (interpret_statement_list (cdr stmt) (pushframe env) ret break cont))
    ))

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


;;; ENVIRONMENT LOGIC

; The environment is stored as a list of frames, ordered by most recent frame
; first. Each frame has two lists of equal size, the first of variable names,
; and the second of their values.

; Generates a new environment. 
(define newenv    (lambda ()        (cons (newframe) '())))

; Frame abstractions.
(define newframe  (lambda ()        '(()())))
(define names     (lambda (frame)   (car frame)))
(define vals      (lambda (frame)   (car (cdr frame))))
(define topframe  (lambda (env)     (car env)))
(define lowframes (lambda (env)     (cdr env)))
(define pushframe (lambda (env)     (cons (newframe) env)))
(define popframe  (lambda (env)     (cdr env)))
(define inframe?  (lambda (x frame) (not (= -1 (getindex x (names frame))))))
(define getval    (lambda (x frame) (itemat (getindex x (names frame)) (vals frame))))

; Gets the item in list l at index i, starting from 0.
(define itemat (lambda (i l)
    (if (= i 0)
      (car l)
      (itemat (- i 1) (cdr l))
      )))

; Removes the item in list l at index i, starting from 0.
(define removeat (lambda (i l)
    (if (= i 0)
      (cdr l)
      (cons (car l) (removeat (- i 1) (cdr l)))
      )))

; Replaces the item in list l at index i with x, starting at 0.
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
          (else (getindex-cps x (cdr l) (lambda (v) (k (+ v 1)))))
          ))))
      (getindex-cps x l (lambda (v) v))
    )))

; Declares a new variable in the environment in the current frame.
(define declare (lambda (name env)
	(cons
	  (cons (cons name (names (topframe env)))
	        (cons (cons 0 (vals (topframe env)))
	               '()))
      (lowframes env)
      )))

; Tests whether the variable is delcared.
(define declared? (lambda (x env)
    (cond
      ((null? env) #f)
      ((inframe? x (topframe env)) #t)
      (else (declared? x (lowframes env)))
      )))

; Sets the value of the named variable in the environment to val in the most current frame.
(define assign (lambda (name val env)
    (if (inframe? name (topframe env))
	  (cons
	    (cons (names (topframe env))
	          (cons (replaceat val (getindex name (names (topframe env))) (vals (topframe env)))
	                 '()))
        (lowframes env))
      (cons (topframe env) (assign name val (lowframes env)))
      )))

; Declares a new variable in the environment in the lowest frame.
(define gDeclare (lambda (name env)
    (cond
      ((null? env) (declare name (newenv)))
      ((and (null? (lowframes env)) (list? (topframe env))) (declare name env))
      (else (cons (topframe env) (gDeclare name (lowframes env))))
      )))

; Tests whether the variable is declared in the lowest frame.
(define gDeclared? (lambda (name env)
    (cond
      ((null? env) #f)
      ((null? (cdr env)) (inframe? name (topframe env)))
      (else (gDeclared? name (lowframes env)))
      )))

; Sets the value of the named variable in the environment to val in the lowest frame.
(define gAssign (lambda (name val env)
    (cond
      ((null? env) (assign name val (newenv)))
      ((and (null? (lowframes env)) (list? (topframe env))) (assign name val env))
      (else (cons (topframe env) (gAssign name val (lowframes env))))
      )))

; Returns the value of the given variable name.
(define lookup (lambda (x env)
    (if (inframe? x (topframe env))
      (getval x (topframe env))
      (lookup x (lowframes env))
      )))

