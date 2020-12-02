#lang racket/base
  
(require racket/file racket/vector racket/string)
(require data/heap)

(provide load-values find-and-multiply)

(define (load-values file-name)
  (let ([values (make-heap <=)])
    ; insert each value into the heap so create a sorted structure, cheaper than
    ; sorting a list/vector after the fact
    (map (λ (s) (heap-add! values (string->number s))) (file->lines file-name))
    (heap->vector values)))

(define (find-and-multiply values sum)
  (let* (; limit is the largest value in the vector that could possibly be valid
         [limit (- sum (vector-ref values 0))]
         ; remove any value greater than limit
         [values (vector-filter (λ (v) (< v limit)) values)]
         [length (- (vector-length values) 1)])
    (let outer ([x 0])
      (when (< x length)
        (let* ([xv (vector-ref values x)]
               [yv (let inner ([y 0])
                     (let* ([yv (vector-ref values y)]
                            [csum (+ xv yv)])
                       (if (and (< y length) (< csum sum))
                           (inner (+ y 1))
                           yv)))]
               [csum (+ xv yv)])
          (cond  [(= csum sum) (list xv yv)]
                 [(< x length) (outer (+ x 1))]
                 [else '()]
                 ))
        ))))
