#lang scheme

(require rackunit)

(define (union list1 list2)
  ;basecase, return list2 if list1 is empty
  (if(null? list1)
     list2
     ;check if the head of list1 is in list2
     (if(member (car list1) list2)
        ;return recursive call if contains (duplicate item discarded)
        (union  (cdr list1) list2)
        ;return the head and recursive call if not (store the non-duplicate item)
        (cons (car list1) (union (cdr list1) list2))
     )
  )
)

(define (intersection list1 list2)
  ;basecase, return null if list1 is empty
  (if(null? list1)
     null
     ;check if the head of list1 is in list2
     (if(member (car list1) list2)
        ;return the head and recursive call if contains (store the shared item)
        (cons (car list1) (intersection (cdr list1) list2))
        ;return recursive call if not (discard the unshared item)
        (intersection (cdr list1) list2)
     )
  )
)



(check-equal? (intersection '(3 4 5) '(1 2 3)) '(3))
(check-equal? (intersection '(5 4 3) '(1 2 3)) '(3))
(check-equal? (intersection '() '(1 2 3)) '())
(check-equal? (intersection '() '()) '())
(check-equal? (intersection '(1 2 3) '()) '())
(check-equal? (intersection '(3 4 5) '(4 3)) '(3 4))
(check-equal? (intersection '(5 4 3) '(4 3)) '(4 3))
(check-equal? (intersection '(4 5 6) '(1 2 3)) '())
(check-equal? (intersection '(1 2 3 4 5 8 10) '(1 2 3 5 8 20)) '(1 2 3 5 8))

(check-equal? (union '(3 4 5) '(1 2 3)) '(4 5 1 2 3))
(check-equal? (union '(5 4 3) '(3 2 1)) '(5 4 3 2 1))
(check-equal? (union '() '(1 2 3)) '(1 2 3))
(check-equal? (union '(1 2 3) '()) '(1 2 3))
(check-equal? (union '() '()) '())
(check-equal? (union '(3 4 5) '(4 3)) '(5 4 3))
(check-equal? (union '(5 4 3) '(3 4)) '(5 3 4))
(check-equal? (union '(4 5 6) '(1 2 3)) '(4 5 6 1 2 3))
(check-equal? (union '(1 2 3 4 5 8 10 20) '(1 2 3 9 10 20 100)) '(4 5 8 1 2 3 9 10 20 100))
