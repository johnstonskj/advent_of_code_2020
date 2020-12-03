#lang racket/base

(require racket/file racket/list racket/vector)
(require "./data.rkt")

(provide load-tree-map count-trees)

(define (load-tree-map file-name)
  (list->vector (load-data-from file-name string->tree-line)))

(define (string->tree-line s)
  (list->vector (map (λ (c) (char=? c #\#)) (string->list s))))

(define (tree-at? map row col)
  (vector-ref (vector-ref map row) col))

(define (count-trees map #:left left #:down down)
  (let ([depth (vector-length map)]
        [width (vector-length (vector-ref map 0))])
    (let loop ([row 0] [col 0] [trees 0])
      (if (< row depth)
          (let* ([tree? (tree-at? map row col)]
                 [next-row (+ row down)]
                 [next-col? (+ col left)]
                 [next-col (if (>= next-col? width)
                               (- next-col? width)
                               next-col?)])
            (loop next-row next-col (if tree? (+ trees 1) trees)))
          trees))))
