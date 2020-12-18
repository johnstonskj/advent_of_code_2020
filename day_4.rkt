#lang racket/base

(require racket/list)

(require "aoc.rkt" "passports.rkt")

(define passport-data (load-passport-data "./day_4_input.txt"))

(answer '(4 . 1)
        (count passport-has-required-keys? passport-data)
        202
        #:msg "number of valid passports")

(answer '(4 . 2)
        (count passport-valid? passport-data)
        137
        #:msg "number of valid passports")
