#lang racket/base
  
(require "./expenses.rkt")

(define (find-sum-of of)
  (display "sum of ")
  (display of)
  (display " items ")
  (let ([result (find-and-multiply (load-expense-values "day_1_input.txt") 2020 of)])
  (if (list? result)
      (displayln (foldl * 1 result))
      (displayln "not found :("))))

; Day 1, part 1
(find-sum-of 2)

; Day 1, part 2
(find-sum-of 3)