; 1) insert takes a number and a list of numbers in order and inserts the
; number in the proper place.
(define insert
  (lambda (n l)
    (cond
      ((null? l) (cons n '()))
      ((> (car l) n) (cons n l))
      (else (cons (car l) (insert n (cdr l))))
      )))

; 2) removedups takes a list of atoms and removes any atom that is a repeat of
; the atom that immediately precedes it.
(define removedups
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      ((eq? (car l) (car (cdr l))) (removedups (cdr l)))
      (else (cons (car l) (removedups (cdr l))))
      )))

; 3) nestlist takes a list of atoms and nests each atom into a nested sublist so
; the last element is the deepest.
(define nestlist
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) (cons (car l) '()))
      (else (cons (car l) (cons (nestlist (cdr l)) '())))
      )))

; 4) deepcons takes an element and a list, that possibly containst sublists and
; places the element in the front of the first element, as deep in the sublist
; as needed.
(define deepcons
  (lambda (a l)
    (cond
      ((null? l) (cons a '()))
      ((list? (car l)) (cons (deepcons a (car l)) (cdr l)))
      (else (cons a l))
      )))

; 5) nestlistfront takes a list of atoms and nests each element so the first
; element is the deepest.
(define nestlistfront
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (deepcons (cons (car l) '()) (nestlistfront (cdr l))))
      )))

; 6) numparens* takes a list and returns the number of pairs of parentheses.
(define numparens*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((list? (car l)) (+ (numparens* (car l)) (numparens* (cdr l))))
      (else (numparens* (cdr l)))
      )))

; 7) dup* takes a list and duplicates all contents, including any sublists.
(define dup*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (dup* (car l)) (cons (dup* (car l)) (dup* (cdr l)))))
      (else (cons (car l) (cons (car l) (dup* (cdr l)))))
      )))

; 8) removedups* takes a list, that can contain sublists, and removes any atom
; that is the repeat of the atom that immediately precedes it in the same
; sublist.
(define removedups*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (removedups* (car l)) (removedups* (cdr l))))
      ((null? (cdr l)) (cons (car l) '()))
      ((eq? (car l) (car (cdr l))) (removedups* (cdr l)))
      (else (cons (car l) (removedups* (cdr l))))
      )))

; 9) removedups** takes a list, that can contain sublists, and removes any
; element that, once repeated elements have been removed from it, is the repeat
; of any element (also once elements have been removed from it) that immediately
; precedes it in the same sublist.
(define atom?
  (lambda (x)
    (and (not (list? x)) (not (pair? x)))
    ))

(define listeq?
  (lambda (l1 l2)
    (cond
      ((or (atom? l1) (atom? l2)) (eq? l1 l2))
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((eq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2)))
      ((and (pair? (car l1)) (pair? (car l2))) (and (listeq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2))))
      (else #f)
      )))

(define removeduplists*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cons (car l) (removeduplists* (cdr l))))
      ((null? (cdr l)) (cons (removeduplists* (car l)) '()))
      ((listeq? (removeduplists* (car l)) (car (removeduplists* (cdr l)))) (removeduplists* (cdr l)))
      (else (cons (removeduplists* (car l)) (removeduplists* (cdr l))))
      )))

(define removedups**
  (lambda (l)
    (removeduplists* (removedups* l))
    ))

; 10) transpose takes a matrix and transposes it by swapping the ith row with
; the ith column. Since the list represents a matrix, you can assume the
; parameter is a list of lists, and each sublist has the same number of
; elements.
(define cars
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (car (car l)) (cars (cdr l))))
      (else (cons (car l) (cars (cdr l))))
      )))

(define cdrs
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) (cons (cdr (car l)) '()))
      (else (cons (cdr (car l)) (cdrs (cdr l))))
      )))

(define null?*
  (lambda (l)
    (cond
      ((null? l) #t)
      ((list? (car l)) (and (null?* (car l)) (null?* (cdr l))))
      (else #f)
      )))

(define transpose
  (lambda (m)
    (cond
      ((null?* m) '())
      (else (cons (cars m) (transpose (cdrs m))))
      )))

; Let's print out something meaningful, not just the last function defined.
(cons '> '(Wes Rupert: EECS 345 Homework Assignment 01))
