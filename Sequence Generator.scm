#lang scheme

(require rackunit)
(require srfi/1)

(define (sequence-generator terms coef)
  
  (lambda(input)
    (define (make-sequence terms input)
       (if(zero? input)
          (car terms)
          ;update the terms and use it as arguments for recursive call, it is tail recursion to avoid stack overflow
          (make-sequence (append (cdr terms) (list (calc-next terms coef))) (- input 1))
       )
    )

    ;helper function to calculate the next element
    (define (calc-next list1 list2)
      (reduce + 0 (map * list1 list2))
    )

    (make-sequence terms input)
  )
)

(define peter (sequence-generator '(5 -3) '(2 1)))
(define fibonacci (sequence-generator '(0 1) '(1 1)))
(define padovan2 (sequence-generator '(1 1 1) '(1 1 0)))
(define lucas (sequence-generator '(2 1) '(1 1)))


;this is for test purpose, keep or delete it as you wish
(fibonacci 24)
(fibonacci 1000)
(lucas 24)
(lucas 1000)
(padovan2 24)
(padovan2 1000)
(peter 24)
(peter 1000)

