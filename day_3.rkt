#lang racket/base

(require racket/list)
(require "./tree-map.rkt")

(define tree-map (load-tree-map "day_3_input.txt"))

(display "Number of trees for 3,1: ")
(displayln (count-trees tree-map #:left 3 #:down 1))

(display "Product of tree count: ")
(displayln
 (foldl * 1
       (map (λ (pair)
              (count-trees tree-map #:left (first pair) #:down (second pair)))
              '((1 1) (3 1) (5 1) (7 1) (1 2)))))