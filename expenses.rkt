#lang racket/base
  
(require racket/file racket/list racket/vector)
(require data/heap)

(provide load-values find-and-multiply)

(define (load-values file-name)
  (let ([values (make-heap <=)])
    ; insert each value into the heap so create a sorted structure, 
    ; cheaper than sorting a list/vector after the fact
    (map (λ (s) (heap-add! values (string->number s))) (file->lines file-name))
    (heap->vector values)))

(define (scan-and-multiply values length sum with of)
  (let next ([x 0])
    (let* ([xv (vector-ref values x)]
           [csum (foldl + xv with)])
      (cond
        ; got a result
        [(and (= csum sum) (= of 1)) (list* xv with)]
        ; no result here, look at the next rank
        [(> of 1)
         (let ([result (scan-and-multiply values length sum (list* xv with) (- of 1))])
           (if (and (empty? result) (< x length))
               (next (+ x 1))
               result))]
        ; no result, but wait, there's more
        [(< x length) (next (+ x 1))]
        ; no result, and nothing left here
        [else '()]))))

;; values - a vector of numbers
;; find-sum - the sum we are looking for
;; of - the number of values to sum and compare to find-sum
(define (find-and-multiply values find-sum of)
  (cond [(<= of 0)
         '()]
        [else
         (let* (; limit is the largest value in the vector that could possibly be valid
                [limit (- find-sum (vector-ref values 0))]
                ; remove any value greater than limit
                [values (vector-filter (λ (v) (<= v limit)) values)])
           (scan-and-multiply values (- (vector-length values) 1) find-sum (list) of))])) 
