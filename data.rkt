#lang racket/base

(require racket/bool racket/file racket/function racket/list racket/string)
(require data/heap)

(provide load-data-from load-and-sort-data-from load-chunked-data-from)

; ------------------------------------------------------------------------------------------

(define (string-empty? s)
  (not (non-empty-string? s)))

;; -> list
(define (load-chunked-data-from
         file-name
         [constructor identity]
         #:to-line? [to-line? #t]
         #:sep? [sep? string-empty?])
  (map constructor
       (let ([lines (file->lines file-name)])
         (let next-break ([rest lines] [break (index-where lines string-empty?)] [chunks '()])
           (if (false? break)
               (if to-line?
                   (map string-join (list* rest chunks))
                   (list* rest chunks))
               (let ([before (take rest break)]
                     [after (drop rest (add1 break))])
                 (next-break after (index-where after string-empty?) (list* before chunks))))))))

;; -> list
(define (load-data-from file-name [constructor identity])
  (map constructor (file->lines file-name)))

;; -> vector
(define (load-and-sort-data-from file-name [constructor identity] [cmp <])
  (let ([values (make-heap cmp)])
    ; insert each value into the heap so create a sorted structure, 
    ; cheaper than sorting a list/vector after the fact
    (map (λ (s) (heap-add! values (constructor s))) (file->lines file-name))
    (heap->vector values)))
