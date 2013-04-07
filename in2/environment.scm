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
(define newframe  (lambda ()        '(()())))
(define names     (lambda (frame)   (car frame)))
(define vals      (lambda (frame)   (car (cdr frame))))
(define topframe  (lambda (env)     (car env)))
(define lowframes (lambda (env)     (cdr env)))
(define pushframe (lambda (env)     (cons (newframe) env)))
(define popframe  (lambda (env)     (cdr env)))
(define inframe?  (lambda (name frame) (not (= -1 (getindex name (names frame))))))
(define getval    (lambda (name frame) (itemat (getindex name (names frame)) (vals frame))))

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
(define replaceat (lambda (name i l)
    (if (= i 0)
      (cons name (cdr l))
      (cons (car l) (replaceat name (- i 1) (cdr l)))
      )))

; Gets the index of the given item.
(define getindex (lambda (name l)
    (letrec
      ((getindex-cps (lambda (name l k)
        (cond
          ((null? l) -1)
          ((eq? name (car l)) (k 0))
          (else (getindex-cps name (cdr l) (lambda (v) (k (+ v 1)))))
          ))))
      (getindex-cps name l (lambda (v) v))
    )))

; Declares a new variable in the environment in the current frame.
(define declare (lambda (name env)
	(cons
      (cons
        (cons name          (names (topframe env)))
	    (cons (cons (box 0) (vals  (topframe env))) '()))
      (lowframes env)
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
    (begin
      (cond
        ((declared? name env) (set-box! (lookup-ref name env) val))
        (else (assign name val (declare name env)))
        )
      env
      )))

; Returns the value of the given variable name.
(define lookup-ref (lambda (name env)
    (if (inframe? name (topframe env))
      (getval name (topframe env))
      (lookup-ref name (lowframes env))
      )))

(define lookup (lambda (name env) (unbox (lookup-ref name env))))
