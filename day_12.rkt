#lang racket/base

(require "aoc.rkt" "navigation.rkt")

(define instructions (load-navigation-instructions "day_12_input.txt"))

(answer '(12 . 2) (navigate instructions) 879)