#lang racket/base
  
(require racket/file racket/list racket/vector racket/string)
(require data/heap)

(provide load-values find-and-multiply)

(define (load-values file-name)
  (let ([values (make-heap <=)])
    ; insert each value into the heap so create a sorted structure, 
    ; cheaper than sorting a list/vector after the fact
    (map (λ (s) (heap-add! values (string->number s))) (file->lines file-name))
    (heap->vector values)))

;(define (scan-and-multiply values sum with of)
;  (let ([length (- (vector-length values) 1)])
;    (let outer ([x 0])
;      (when (< x length)
;        (let* ([xv (vector-ref values x)]
;               [yv (let inner ([y 0])
;                     (let* ([yv (vector-ref values y)]
;                            [csum (+ xv yv)])
;                       (if (and (< y length) (< csum sum))
;                           (inner (+ y 1))
;                           yv)))]
;               [csum (+ xv yv)])
;          (cond  [(= csum sum) (list xv yv)]
;                 [(< x length) (outer (+ x 1))]
;                 [else '()]
;                 ))
;        ))))

(define (scan-and-multiply values length sum with of)
  (let next ([x 0])
    (let* ([xv (vector-ref values x)]
           [csum (foldl + xv with)])
      (cond
        [(and (= csum sum) (= of 1)) (list* xv with)]
        [(> of 1)
                  (let ([result (scan-and-multiply values length sum (list* xv with) (- of 1))])
                    (if (and (empty? result) (< x (- length 1)))
                        (next (+ x 1))
                        result))]
        [(< x (- length 1)) (next (+ x 1))]
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
