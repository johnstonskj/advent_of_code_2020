#lang racket/base

(require racket/list)

(require "aoc.rkt" "chargers.rkt")

; ------------------------------------------------------------------------------------------

(define charger-data (load-charger-jolts "day_10_input.txt"))
(define diffs (jolt-diffs charger-data))

(answer '(10 . 1)
        (* (count (λ (v) (= v 1)) diffs)
              (count (λ (v) (= v 3)) diffs))
        2277
        #:msg "sum of diff-1 and diff-3")

(answer '(10 . 2)
        (charger-paths diffs)
        37024595836928
        #:msg "distinct arrangements")