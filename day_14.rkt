#lang racket/base

(require "aoc.rkt" "pcs.rkt")

(define pcs-data (load-pcs-initialization "day_14_input.txt"))

(answer '(14 . 1)
        (apply + (hash-values (initialize-memory-v1 pcs-data)))
        15919415426101)

(answer '(14 . 2)
        (apply + (hash-values (initialize-memory-v2 pcs-data)))
        )
