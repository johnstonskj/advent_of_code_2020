#lang racket/base

(require "./bags.rkt" racket/list racket/set racket/string)

(define bag-data (load-bag-data "./day_7_input.txt"))

(display "Number of bags containing a shiny gold bag: ")
(set-count (find-all-containing bag-data "shiny gold"))


(display "Number of bags contained within a shiny gold bag: ")
(find-all-contained-within bag-data "shiny gold")