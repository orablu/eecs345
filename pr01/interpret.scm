(define interpret
  (lambda (file)
    (
     (lookup 'return (interpret_statement_list (parser file) (newenvironment))
     )))

(define interpret_statement_list
  (lambda (parsetree environment)
    (cond
     ((null? parsetree) environment)
     (else (interpret_statement_list (cdr parsetree) (interpret_statement (car parsetree environment))))
     )))

(define interpret_statement
  (lambda (statement environment)
    (cond
      ((eq? '=      (operator statement)) (interpret_assign  statement environment))
      ((eq? 'var    (operator statement)) (interpret_declare statement environment))
      ((eq? 'if     (operator statement)) (interpret_if      statement environment))
      ((eq? 'return (operator statement)) (interpret_return  statement environment))
     )))

(define expression?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((number? expr) #t)
      ((not (pair? expr)) #f)
      ((null? (cdr expr)) (expression? (car expr)))
      ((not (eq? (length expr) 3)) #f)
      ((or (eq? '+ (operator expr)) (eq? '- (operator expr)) (eq? '* (operator expr)) (eq? '/ (operator expr)) (eq? '% (operator expr))) (and (expression? (op1 expr)) (expression? (op2 expr))))
      (else #f)
      )))

(define value
  (lambda (expr env)
    (cond
      ((number? expr) expr)
      ((not (pair? expr)) (lookup expr env))
      ((null? (cdr expr)) (value (car expr) env))
      ((eq? '+ (operator expr)) (+         (value (op1 expr) env) (value (op2 expr) env)))
      ((eq? '- (operator expr)) (-         (value (op1 expr) env) (value (op2 expr) env)))
      ((eq? '* (operator expr)) (*         (value (op1 expr) env) (value (op2 expr) env)))
      ((eq? '/ (operator expr)) (quotient  (value (op1 expr) env) (value (op2 expr) env)))
      ((eq? '% (operator expr)) (remainder (value (op1 expr) env) (value (op2 expr) env)))
      (else #f); TODO: Throw an error
      )))

(define lookup
  (lambda (x env)
    (cond
      ((null? env) #f); TODO: Throw an error
      ((not (pair? (car env))) #f); TODO: Throw an error
      ((eq? x (car (car env))) (cdr (car env)))
      (else (lookup x (cdr env)))
      )))

(define interpret_return
  (lambda (statement environment)
    (putlast 'return (op1 statement) environment)
     )))

(define put (lambda (name val env) (cons (cons name val) env)))
(define pulast (lambda (name val env) (append env (cons '() (cons name val)))))
(define op1 (lambda (expr) (car expr)))
(define op2 (lambda (expr) (car (cdr (cdr expr)))))
(define operator (lambda (expr) (car (cdr expr))))
