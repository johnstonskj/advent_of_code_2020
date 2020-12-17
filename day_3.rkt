#lang racket/base

(require racket/list)
(require "aoc.rkt" "tree-map.rkt")

(define tree-map (load-tree-map "day_3_input.txt"))

(answer '(3 . 1)
        (count-trees tree-map #:right 3 #:down 1)
        209
        #:msg "number of trees") 

(answer '(3 . 2)
        (foldl * 1
               (map (λ (pair)
                      (count-trees tree-map #:right (first pair) #:down (second pair)))
                    '((1 1) (3 1) (5 1) (7 1) (1 2))))
        1574890240
        #:msg "product of tree count")