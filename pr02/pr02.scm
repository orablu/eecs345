; Wes Rupert - wkr3
; EECS 345 - Program 2


; For quick reference to the identity function when debugging. Not used in the actual code.
(define i (lambda (v) v))


; 1. A function duplicate that takes two argument, an element and a size, and creates a list of the requested size.
    (define duplicate
      (lambda (e n)
        (cond
          ((< n 1) '())
          (else (cons e (duplicate e (- n 1))))
          )))

    (define duplicate-cps
      (lambda (e n k)
        (cond
          ((< n 1) (k '()))
          (else (duplicate-cps e (- n 1) (lambda (v) (k (cons e v)))))
          )))


; 2. A function removedups that takes a list and removes any atom that is a repeat of the atom that immediately precedes it.
    (define removedups
      (lambda (l)
        (cond
          ((null? l) '())
          ((null? (cdr l)) l)
          ((eq? (car l) (car (cdr l))) (removedups (cdr l)))
          (else (cons (car l) (removedups (cdr l))))
          )))

    (define removedups-cps
      (lambda (l k)
        (cond
          ((null? l) (k '()))
          ((null? (cdr l)) (k l))
          ((eq? (car l) (car (cdr l))) (removedups-cps (cdr l) (k (cdr l))))
          (else (removedups-cps (cdr l) (lambda (v) (k (cons (car l) v)))))
          )))


; 3. The function count* that takes a list and and an element and returns the number of occurrences of the element in the list and all its sublists.
    (define count*
      (lambda (a l)
        (cond
          ((null? l) 0)
          ((list? (car l)) (+ (count* a (car l)) (count* a (cdr l))))
          ((eq? a (car l)) (+ 1 (count* a (cdr l))))
          (else (count* a (cdr l)))
          )))

    (define count-cps*
      (lambda (a l k)
        (cond
          ((null? l) (k 0))
          ((list? (car l)) (count-cps* a (car l) (lambda (v1) (k (count-cps* a (cdr l) (lambda (v2) (+ v1 v2)))))))
          ((eq? a (car l)) (count-cps* a (cdr l) (lambda (v) (k (+ 1 v)))))
          (else (count-cps* a (cdr l) k))
          )))


; 4. The function numbersonly? that takes a list and returns if the list contains only numbers.
    (define numbersonly?
      (lambda (l)
        (cond
          ((null? l) #t)
          ((number? (car l)) (numbersonly? (cdr l)))
          (else #f)
          )))

    (define numbersonly-cps?
      (lambda (l k)
        (cond
          ((null? l) (k #t))
          ((number? (car l)) (numbersonly-cps? (cdr l) k))
          (else (k #f))
          )))


; 5. The function cleannumbers that takes a list of lists and returns a list that contains only those sublists that contain only numbers.
    (define cleannumbers
      (lambda (l)
        (cond
          ((null? l) '())
          ((numbersonly? (car l)) (cons (car l) (cleannumbers (cdr l))))
          (else (cleannumbers (cdr l)))
          )))

    (define cleannumbers-cps
      (lambda (l k)
        (cond
          ((null? l) (k '()))
          ((numbersonly-cps? (car l) (lambda (v) (if v
            (cleannumbers-cps (cdr l) (lambda (v1) (k (cons (car l) v1))))
            (cleannumbers-cps (cdr l) k)))))
          )))


; 6. The function merge that merges two sorted lists of numbers into a larger sorted list.
    (define merge
      (lambda (l1 l2)
        (cond
          ((null? l1) l2)
          ((null? l2) l1)
          ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
          (else (cons (car l2) (merge l1 (cdr l2))))
          )))

    (define merge-cps
      (lambda (l1 l2 k)
        (cond
          ((null? l1) (k l2))
          ((null? l2) (k l1))
          ((< (car l1) (car l2)) (merge-cps (cdr l1) l2 (lambda (v) (k (cons (car l1) v)))))
          (else (merge-cps l1 (cdr l2) (lambda (v) (k (cons (car l2) v)))))
          )))


; 7. The function evens that takes a boolean and a list. If the boolean is true, evens will return every element at an even index, and if the boolean is false, evens will return every element at an odd index.
    (define evens
      (lambda (b l)
        (cond
          ((null? l) '())
          ((not b) (cons (car l) (evens (not b) (cdr l))))
          (else (evens (not b) (cdr l)))
          )))

    (define evens-cps
      (lambda (b l k)
        (cond
          ((null? l) (k '()))
          ((not b) (evens-cps (not b) (cdr l) (lambda (v) (k (cons (car l) v)))))
          (else (evens-cps (not b) (cdr l) k))
          )))


; 8. The function Mergesort that takes a list of numbers and returns a sorted version. If you recall the merge sort algorithm, you call evens twice to get lists containing the elements and the even and odd indeces, you then recursively call mergesort on each sublist, and then you call merge on the two lists returned by the recursive calls to mergesort.
    (define Mergesort
      (lambda (l)
        (cond
          ((null? l) '())
          ((null? (cdr l)) l)
          (else (merge (Mergesort (evens #t l)) (Mergesort (evens #f l))))
          )))

    (define Mergesort-cps
      (lambda (l k)
        (cond
          ((null? l) (k '()))
          ((null? (cdr l)) (k l))
          (else (evens-cps #t l (lambda (v1)
            (evens-cps #f l (lambda (v2)
              (Mergesort-cps v1 (lambda (v3)
                (Mergesort-cps v2 (lambda (v4)
                  (merge-cps v3 v4 k))))))))))
          )))


; 9. Use continuation passing style to create the following function without using external helper functions and without adding new parameters. The function split takes a list and returns a list containing two sublists, the first with the elements at the even indices and the second with the elements at the odd indices:
    (define split-nocps ; For figuring out the problem, not used in real split method.
      (lambda (b l)
        (cond
          ((null? l) '(()()))
          (b (cons
               (cons
                 (car l)
                 (car (split-nocps (not b) (cdr l))))
               (cons
                 (car (cdr (split-nocps (not b) (cdr l))))
                 '())))
          (else (cons
                  (car (split-nocps (not b) (cdr l)))
                  (cons
                    (cons
                      (car l)
                      (car (cdr (split-nocps (not b) (cdr l)))))
                    '())))
          )))

    (define split
      (lambda (l)
        (letrec ((split-cps (lambda (b l k) (cond
          ((null? l) (k '(()())))
          (b (split-cps (not b) (cdr l) (lambda (v)
            (k (cons (cons (car l) (car v)) (cons (car (cdr v)) '()))))))
          (else (split-cps (not b) (cdr l) (lambda (v)
            (k (cons (car v) (cons (cons (car l) (car (cdr v))) '()))))))
          ))))
          (split-cps #f l (lambda (v) v))
          )))


; 10. Write the following function without external helper functions or additional parameters. You do not need to use continuation passing style, but you may use continuations or call-with-current-continuation to assist you. The function suffix takes an atom and a list and returns a list containing all elements that occur after the last occurrence of the atom.
    (define suffix
      (lambda (a l k)
        (cond
          ((null? l) (k '()))
          ((eq? a (car l)) (suffix a (cdr l) (lambda (v) v)))
          (else (suffix a (cdr l) (lambda (v) (k (cons (car l) v)))))
          )))

