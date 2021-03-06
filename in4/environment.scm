;;; ENVIRONMENT LOGIC

 #| Methods to use:
  | - (env) newenv     : 
  | - (env) pushframe  : env
  | - (env) popframe   : env
  | - (env) declare    : name env
  | - (env) assign     : name val env
  | - (val) lookup     : name env
  | - (ref) lookup-ref : name env
  |#

; The environment is stored as a list of frames, ordered by most recent frame
; first. Each frame has two lists of equal size, the first of variable names,
; and the second of their values.

; Generates a new environment. 
(define newenv    (lambda ()        (cons (newframe) '())))

; Frame abstractions.
(define newframe  (lambda ()        (box '(()()))))
(define names     (lambda (frame)   (car (unbox frame))))
(define vals      (lambda (frame)   (car (cdr (unbox frame)))))
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
    (if (not (inframe? name (topframe env)))
      (begin
        (set-box! (topframe env) (list
          (cons name    (names (topframe env)))
          (cons (box 0) (vals  (topframe env)))))
        env)
      (error "Variable is already defined:" name)
      )))

; Tests whether the variable is delcared.
(define declared? (lambda (name env)
    (cond
      ((null? env) #f)
      ((inframe? name (topframe env)) #t)
      (else (declared? name (lowframes env)))
      )))

; Sets the value of the named variable in the environment to val in the most current frame.
(define assign (lambda (name val env)
    (if (declared? name env)
      (begin
        (set-box! (lookup-ref name env) val)
        env)
      (error "Referencing undeclared variable:" name)
      )))

; Returns the reference to the given variable.
(define lookup-ref (lambda (name env)
    (if (inframe? name (topframe env))
      (getval name (topframe env))
      (lookup-ref name (lowframes env))
      )))

; Returns the value of the given variable.
(define lookup (lambda (name env) (unbox (lookup-ref name env))))
