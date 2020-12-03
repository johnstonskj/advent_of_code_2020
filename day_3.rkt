#lang racket/base

(require "./tree-map.rkt")

(define tree-map (load-tree-map "day_3_input.txt"))

(count-trees tree-map #:left 3 #:down 1)