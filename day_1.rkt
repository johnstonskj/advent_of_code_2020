#lang racket/base
  
(require "aoc.rkt" "expenses.rkt")

(define expenses (load-expense-values "day_1_input.txt"))

(define (find-sum-of of)
  (let ([result (find-and-multiply expenses 2020 of)])
    (if (list? result)
        (foldl * 1 result)
        #f)))

(answer '(1 . 1) (find-sum-of 2) 988771)

(answer '(1 . 2) (find-sum-of 3) 171933104)