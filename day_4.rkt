#lang racket/base

(require "aoc.rkt" "passports.rkt")

(define passport-data (load-passport-data "./day_4_input.txt"))

(answer '(4 . 1)
        (length (filter passport-valid? passport-data))
        137
        #:msg "number of valid passports")
