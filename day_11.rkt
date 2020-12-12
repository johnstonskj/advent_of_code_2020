#lang racket/base

(require "seating.rkt")

(define seat-map (load-seat-map "day_11_input.txt"))

(display "number of seated at stable: ")
(displayln (find-stable-seating seat-map))