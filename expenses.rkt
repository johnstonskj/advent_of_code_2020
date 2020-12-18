#lang racket/base
  
(require racket/file racket/list racket/vector)
(require "./data.rkt")

(provide load-expense-values find-and-multiply)

(define (load-expense-values file-name)
  (load-and-sort-data-from file-name string->number <=))

(define (scan-and-multiply values length sum with of)
  (let next ([x 0])
    (let* ([xv (vector-ref values x)]
           [csum (foldl + xv with)])
      (cond
        ; got a result
        [(and (= csum sum) (= of 1)) (list* xv with)]
        ; no result here, look at the next rank
        [(> of 1)
         (let ([result (scan-and-multiply values length sum (list* xv with) (sub1 of))])
           (if (and (empty? result) (< x length))
               (next (add1 x))
               result))]
        ; no result, but wait, there's more
        [(< x length) (next (add1 x))]
        ; no result, and nothing left here
        [else '()]))))

;; values - a vector of numbers
;; find-sum - the sum we are looking for
;; of - the number of values to sum and compare to find-sum
(define (find-and-multiply values find-sum of)
  (cond [(or (> of (vector-length values)) (<= of 0))
         '()]
        [else
         (let* (; limit is the largest value in the vector that could possibly be valid
                [limit (- find-sum (vector-ref values 0))]
                ; remove any value greater than limit
                [values (vector-filter (λ (v) (<= v limit)) values)])
           (scan-and-multiply values (- (vector-length values) 1) find-sum (list) of))])) 

; Cleaner, and simpler, but not optimized
(define (brute-force-3 values find-sum)
  (for*/first ([x values] [y values] [z values]        
                          #:when (= find-sum (+ x y z)))
    (* x y z)))