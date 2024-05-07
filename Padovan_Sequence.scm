#lang scheme

(require rackunit)
(require srfi/1)

;basic problem
(define (padovan input)
  (if(< input 3)
     1
     ;use let to restrict a local binding, and make it tail recursion to avoid stack overflow
     (let pad ((indice 3) (ind-3 1) (ind-2 1) (ind-1 1))
       (if(= indice input)
          (+ ind-3 ind-2)
          (pad (+ indice 1) ind-2 ind-1 (+ ind-3 ind-2) )
       )
     )   
  )
)



;this is for test purpose, keep or delete it as you wish
(padovan 24)
(padovan 1000)

