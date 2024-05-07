#lang scheme

(require srfi/1)
(require rackunit)

(define (neighbours square)
  ;edge case, empty square
  (if(null? square)
     '()
     ;not empty, start from the node on the left of the center node
     (let ((x (car square)) (y (cdr square)) (left (cons( - (car square) 1) (cdr square))))
       (let find_neighbours ((current left) (production '()))
         (if(and (= x (car current)) (= y (cdr current)))
            ;if it's the center node, only takes the nodes above and below it
            (find_neighbours (cons (+ 1 (car current)) y) (append production (list(cons x (+ 1 y)) (cons x (- y 1)))))
            (if (< (- (car current) x) 2)
                ;if it's not the center node, takes the three nodes in one column
                (find_neighbours (cons (+ 1 (car current)) y) (append production (list (cons (car current) (+ 1 y)) current (cons (car current) (- y 1)))))                    
                ;return the production list when it reaches the edge
                production
            )
         )        
       )
     )
  )
)


(define (neighbourhood live-squares)
  ;helper function that finds all the neighbors using the previous function
  (define (find_all live-squares)
     (let find_all_nodes ((squares live-squares) (production '()))
          (if(null? squares)
             production
             (find_all_nodes (cdr squares) (append production (neighbours (car squares))))
          )
     )
  )
  
  (define rawlist1 (find_all live-squares))
  ;add the original nodes into the rawlist
  (define rawlist2 (append live-squares rawlist1))

  ;remove the duplicated nodes
  (let find_neighbourhood((rawlist rawlist2) (production '()))
    (if(null? rawlist)
       production
       (if(member (car rawlist) production)
          (find_neighbourhood(cdr rawlist) production)
          (find_neighbourhood(cdr rawlist) (append production (list(car rawlist)) ))
       )
    )
  )
)


(define (will-be-alive? living-squares square)
  ;citation: this is my intersection function implemented for assignment 6
  (define (intersection list1 list2)
    (if(null? list1)
       null
       (if(member (car list1) list2)
          (cons (car list1) (intersection (cdr list1) list2))
          (intersection (cdr list1) list2)
       )
     )
   )

  ;find the intersection of the neighbors and the given squares
  (define intersect (intersection (neighbours square) living-squares))
  ;it lives or dies according to the rule
  (if (member square living-squares)
      ;rule 1, 2, 3 for living cells
      (if(or (= 2 (length intersect)) (= 3 (length intersect)))
        #t
        #f
      )
      ;rule 4, for dead cells
      (if(= 3 (length intersect))
        #t
        #f)
  )
  
)


(define (next-generation living-squares)
  ;find all the nodes that need to be considered
  (define all_nodes (neighbourhood living-squares))
  ;add them to the production list according to the rule
  (let find-next-gen ((squares all_nodes) (production '()))
    (if(null? squares)
       production
       (if(will-be-alive? living-squares (car squares))
          (find-next-gen (cdr squares) (append (list(car squares)) production))
          (find-next-gen (cdr squares) production)
       )
    )
  )
)



;below is for test purpose, delete them if you wish
#|
(neighbours '())
(neighbours '(3 . 7))
(neighbours '(0 . 0))
(neighbours '(1 . 1))

(neighbourhood '())
(neighbourhood '((-1 . 0) (0 . 0) (1 . 0)))


(will-be-alive? '((0 . 0)) '(0 . 0))
(define r-pentomino '((0 . -1) (-1 . 0) (0 . 0) (0 . 1) (1 . 1)))
(will-be-alive? r-pentomino '(0 . -1))
(will-be-alive? r-pentomino '(0 . 0))
(will-be-alive? r-pentomino '(-1 . -1))
(will-be-alive? r-pentomino '(1 . 0))
(will-be-alive? r-pentomino '())
(will-be-alive? '() '())
(will-be-alive? '() '(1 . 1))

(next-generation '())
(next-generation '((0 . 0)))
(next-generation '((-1 . 0) (0 . 0) (1 . 0)))
(next-generation (next-generation '((-1 . 0) (0 . 0) (1 . 0))))
(next-generation '((-2 . 0) (-1 . 0) (0 . 0) (1 . 0) (2 . 0)))
(next-generation (next-generation '((-2 . 0) (-1 . 0) (0 . 0) (1 . 0) (2 . 0))))
|#