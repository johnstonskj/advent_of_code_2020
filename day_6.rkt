#lang racket/base

(require "aoc.rkt" "customs.rkt")

(define group-data (load-customs-data "./day_6_input.txt"))

(answer '(6 . 1)
        (foldr + 0 (map group-union-count group-data))
        7110
        #:msg "union")

(answer '(6 . 2)
        (foldr + 0 (map group-intersect-count group-data))
        3628
        #:msg "intersect")

