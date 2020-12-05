#lang racket/base

(require "./airplane.rkt")

(define ap (make-airplane 128 8))

(define tickets (load-ticket-data "day_5_input.txt"))

(map (λ (t) (seat-num (ticket->seat ap t))) tickets)

(display "max ticket number: ")
(displayln (foldr max 0 (map (λ (t) (seat-num (ticket->seat ap t))) tickets)))
