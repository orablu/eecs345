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
      (else (duplicate e (- n 1) (lambda (v) (cons e k))))
      )))

; 2. A function removedups that takes a list and removes any atom that is a repeat of the atom that immediately precedes it.
(define removedups
  (lambda (l)
    (cond
      )))

(define removedups-cps
  (lambda (l k)
    (cond
      )))

; 3. The function count* that takes a list and and an element and returns the number of occurrences of the element in the list and all its sublists.
(define count*
  (lambda (a l)
    (cond
      )))

(define count-cps*
  (lambda (a l k)
    (cond
      )))

; 4. The function numbersonly? that takes a list and returns if the list contains only numbers.
(define nmbersonly?
  (lambda (l)
    (cond
      )))

(define numbersonly-cps?
  (lambda (l k)
    (cond
      )))

; 5. The function cleannumbers that takes a list of lists and returns a list that contains only those sublists that contain only numbers.
(define cleannumbers
  (lambda (l)
    (cond
      )))

(define cleannumbers-cps
  (lambda (l k)
    (cond
      )))


; 6. The function merge that merges two sorted lists of numbers into a larger sorted list.
(define merge
  (lambda (l1 l2)
    (cond
      )))

(define merge-cps
  (lambda (l1 l2 k)
    (cond
      )))


; 7. The function evens that takes a boolean and a list. If the boolean is true, evens will return every element at an even index, and if the boolean is false, evens will return every element at an odd index.
(define evens
  (lambda (b l)
    (cond
      )))

(define evens-cps
  (lambda (b l k)
    (cond
      )))


; 8. The function Mergesort that takes a list of numbers and returns a sorted version. If you recall the merge sort algorithm, you call evens twice to get lists containing the elements and the even and odd indeces, you then recursively call mergesort on each sublist, and then you call merge on the two lists returned by the recursive calls to mergesort.
(define Mergesort
  (lambda (l)
    (cond
      )))

(define Mergesort-cps
  (lambda (l k)
    (cond
      )))


; 9. Use continuation passing style to create the following function without using external helper functions and without adding new parameters. The function split takes a list and returns a list containing two sublists, the first with the elements at the even indices and the second with the elements at the odd indices:
(define split
  (lambda (l)
    (cond
      )))

(define split-cps
  (lambda (l k)
    (cond
      )))


; 10. Write the following function without external helper functions or additional parameters. You do not need to use continuation passing style, but you may use continuations or call-with-current-continuation to assist you. The function suffix takes an atom and a list and returns a list containing all elements that occur after the last occurrence of the atom.
(define suffix
  (lambda (a l)
    (cond
      )))

