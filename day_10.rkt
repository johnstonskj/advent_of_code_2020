#lang racket/base

(require racket/list)

(require "./chargers.rkt")

; ------------------------------------------------------------------------------------------

(define charger-data (load-charger-jolts "day_10_input.txt"))
(define diffs (jolt-diffs charger-data))

(display "sum of diff-1 and diff-3: ")
(displayln (* (count (λ (v) (= v 1)) diffs)
              (count (λ (v) (= v 3)) diffs)))

(display "Total distinct arrangements: ")
(displayln (charger-paths diffs))