#lang racket/base

(require "aoc.rkt" "navigation.rkt")

(define instructions (load-navigation-instructions "day_12_input.txt"))

(answer '(12 . 1) (navigate instructions) 879)

(answer '(12 . 2) (navigate/with-waypoint instructions '(1 . 10)) 18107)
