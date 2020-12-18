#lang racket/base

(require "aoc.rkt" "pcs.rkt")

(define pcs-data (load-pcs-initialization "day_14_input.txt"))

(answer '(13 . 1) (apply + (hash-values (initialize-memory pcs-data))) 15919415426101)
