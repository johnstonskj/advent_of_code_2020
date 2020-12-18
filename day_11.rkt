#lang racket/base

(require "aoc.rkt" "seating.rkt")

(define seat-map (load-seat-map "day_11_input.txt"))

(answer '(11 . 1)  (find-stable-seating seat-map count-occupied-neighbors 4) 2424)

(answer '(11 . 2) (find-stable-seating seat-map count-occupied-visible 5) 2208)