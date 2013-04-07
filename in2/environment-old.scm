;;; ENVIRONMENT LOGIC

 #| Methods to use:
  | - (env) newenv     : 
  | - (env) pushframe  : env
  | - (env) popframe   : env
  | - (env) declare    : name env
  | - (env) assign     : name val env
  | - (val) lookup     : name env
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

; Returns the value of the given variable name.
(define lookup (lambda (x env)
    (if (inframe? x (topframe env))
      (getval x (topframe env))
      (lookup x (lowframes env))
      )))

