#lang racket/base

(require "./customs.rkt")

(define group-data (load-customs-data "./day_6_input.txt"))

(display "Sum of all union yes: ")
(displayln (foldr + 0 (map group-union-count group-data)))

(display "Sum of all intersecting yes: ")
(displayln (foldr + 0 (map group-intersect-count group-data)))

