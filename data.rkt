#lang racket/base

(require racket/file)
(require data/heap)

(provide load-data-from load-and-sort-data-from)

;; -> list
(define (load-data-from file-name constructor)
  (map constructor (file->lines file-name)))

;; -> vector
(define (load-and-sort-data-from file-name constructor cmp)
  (let ([values (make-heap cmp)])
    ; insert each value into the heap so create a sorted structure, 
    ; cheaper than sorting a list/vector after the fact
    (map (λ (s) (heap-add! values (constructor s))) (file->lines file-name))
    (heap->vector values)))
