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
        ; no result here, look at the next ranl
        [(> of 1)
         (let ([result (scan-and-multiply values length sum (list* xv with) (- of 1))])
           (if (and (empty? result) (< x (- length 1)))
               (next (+ x 1))
               result))]
        ; no result, but wait, there's more
        [(< x (- length 1)) (next (+ x 1))]
        ; no result, and nothing left here
        [else '()]))))

(define (find-and-multiply values sum of)
  (cond [(<= of 0)
         '()]
        [else
         (let* (; limit is the largest value in the vector that could possibly be valid
                [limit (- sum (vector-ref values 0))]
                ; remove any value greater than limit
                [values (vector-filter (λ (v) (<= v limit)) values)])
           (scan-and-multiply values (vector-length values) sum (list) of))])) 
