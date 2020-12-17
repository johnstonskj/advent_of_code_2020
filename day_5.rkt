#lang racket/base

(require racket/list)

(require "aoc.rkt" "airplane.rkt")

(define ap (make-airplane 128 8))

(define tickets (load-ticket-data "day_5_input.txt"))

(define seats (map (λ (t) (seat-num (ticket->seat ap t))) tickets))

(answer '(5 . 1) (foldr max 0 seats) 864 #:msg "max ticket number")

(define (seat-pairs seats)
  (cond [(eq? seats '()) '()]
        [(< (length seats) 2) '()]
        [(cons (take seats 2) (seat-pairs (cdr seats)))]))

(answer '(5 . 2)
        (filter (λ (p) (= (- (second p)  (first p)) 2)) (seat-pairs (sort seats <)))
        '((738 740))
        #:msg "seat between")

