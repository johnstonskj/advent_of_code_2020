#lang racket/base

(require racket/list racket/set racket/string)

(require "aoc.rkt" "bags.rkt")

(define bag-data (load-bag-data "./day_7_input.txt"))

(answer '(7 . 1)
        (set-count (find-all-containing bag-data "shiny gold"))
        208
        #:msg "bags containing a shiny gold")

(answer '(7 . 2)
        (find-all-contained-within bag-data "shiny gold")
        1664
        #:msg "bags contained within a shiny gold")