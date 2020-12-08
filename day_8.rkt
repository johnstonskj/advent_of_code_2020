#lang racket/base

(require racket/set)

(require "./boot-code.rkt")

(define memory (load-and-assemble-code "day_8_input.txt"))

(define (make-break-on-loop)
  (let ([seen-pc (mutable-set)])
    (λ (pc inst oper)
      (cond
        [(set-member? seen-pc pc)
         #f]
        [else
         (set-add! seen-pc pc)
         #t]))))

(display "value of acumulator: "
(displayln (execute memory #:break (make-break-on-loop)))