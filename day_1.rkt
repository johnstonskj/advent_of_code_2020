#lang racket/base
  
(require "./expenses.rkt")

(let ([result (find-and-multiply (load-values "day_1_input.txt") 2020)])
  (if (list? result)
      (displayln (foldl * 1 result))
      (displayln "not found :(")))
