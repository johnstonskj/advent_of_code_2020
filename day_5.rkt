#lang racket/base

(require racket/list)

(require "./airplane.rkt")

(define ap (make-airplane 128 8))

(define tickets (load-ticket-data "day_5_input.txt"))

(define seats (map (λ (t) (seat-num (ticket->seat ap t))) tickets))

(display "max ticket number: ")
(displayln (foldr max 0 seats))

(define (seat-pairs seats)
  (cond [(eq? seats '()) '()]
        [(< (length seats) 2) '()]
        [(cons (take seats 2) (seat-pairs (cdr seats)))]))

(display "seat between: ")
(displayln (filter (λ (p) (= (- (second p)  (first p)) 2)) (seat-pairs (sort seats <))))

