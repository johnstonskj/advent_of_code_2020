#lang racket/base

(require racket/file racket/list racket/string)

(provide ; Airplane
 *invalid-configuration*
 make-airplane
 airplane-rows
 airplane-cols
 ; Seats
 *invalid-seat*
 make-seat
 seat-col
 seat-row
 seat-num
 ; Ticket
 load-ticket-data
 *invalid-ticket*
 valid-ticket?
 ticket->seat)

; ------------------------------------------------------------------------------------------

(struct airplane
  (rows rdiv cols cdiv) #:transparent)

(define *invalid-configuration* 'INVALID-AIRPLANE-CONFIGURATION)

(define *max-rows* 500)

(define *max-cols* 25)

(define (make-airplane rows cols)
  (if (and (> rows 0)
           (< rows *max-rows*)
           (> cols 0)
           (< cols *max-cols*))
      (airplane rows (inexact->exact (log rows 2)) cols (inexact->exact (log cols 2)))
      *invalid-configuration*))

; ------------------------------------------------------------------------------------------

(struct seat
  (row col num) #:transparent)

(define *invalid-seat* 'INVALID-SEAT)

(define (make-seat ap row col)
  (if (and (>= row 0)
           (< row (airplane-rows ap))
           (>= col 0)
           (< col (airplane-cols ap)))
      (seat row col (+ (* row 8) col))
      *invalid-seat*))

; ------------------------------------------------------------------------------------------

(define (load-ticket-data file-name)
  (file->lines file-name))

(define *fore* #\F)
(define *back* #\B)
(define *left* #\L)
(define *right* #\R)

(define *invalid-ticket* 'INVALID-TICKET)

(define (valid-ticket? ap ticket)
  (let ([chars (string->list ticket)])
    (and
     (= (length chars) (+ (airplane-rdiv ap) (airplane-cdiv ap))) 
     (for/and ([c (take chars (airplane-rdiv ap))]) (or (char=? c *back*) (char=? c *fore*)))
     (for/and ([c (drop chars (airplane-rdiv ap))]) (or (char=? c *left*) (char=? c *right*))))))

(define (ticket->seat ap ticket)
  (if (not (valid-ticket? ap ticket))
      *invalid-ticket*
      (let*  ([chars (string->list ticket)]
              [rows (take chars (airplane-rdiv ap))]
              [cols (drop chars (airplane-rdiv ap))])
        (make-seat ap
                   (bsp-find rows *fore* *back* 0 (sub1 (airplane-rows ap)))
                   (bsp-find cols *left* *right* 0 (sub1 (airplane-cols ap)))))))

(define (bsp-find lst low high min max)
  (if (= (length lst) 1)
      (if (char=? (first lst) low)
          min
          max)
      (if (char=? (first lst) low)
          (bsp-find (rest lst) low high min (inexact->exact (+ min (floor (/ (- max min) 2)))))
          (bsp-find (rest lst) low high (inexact->exact (+ min (ceiling (/ (- max min) 2)))) max))))

(define (seat->ticket ap seat)
  (string-join
   (list (bsp-encode (seat-row seat) *fore* *back* 0 (- (airplane-rows ap) 1))
         (bsp-encode (seat-col seat) *left* *right* 0 (- (airplane-cols ap) 1)))
   ""))

(define (bsp-encode n low high min max [path '()])
  (cond
    [(= min max n) (reverse (list* low path))]
    [(= (- max min) 1)
     (list->string (reverse (if (= min n)
                                (list* low path)
                                (list* high path))))]
    [else
     (let ([mid (inexact->exact (+ min (/ (- max min) 2)))])
       (if (< n mid)
           (bsp-encode n low high min (floor mid) (list* low path))
           (bsp-encode n low high (ceiling mid) max (list* high path)))
       )]))

; ------------------------------------------------------------------------------------------

(require rackunit)

(define airplane-tests
  (test-suite
   "Airplane test suite"
   
   (test-case
    "Check make airplane"
    (let ([ap (make-airplane 128 8)])
      (check-eq? (airplane-rows ap) 128)
      (check-eq? (airplane-rdiv ap) 7)
      (check-eq? (airplane-cols ap) 8)
      (check-eq? (airplane-cdiv ap) 3)))

   (test-case
    "Check ticket valid"
    (let ([*test-ap* (make-airplane 128 8)])
      (check-true (valid-ticket? *test-ap* "FBFBBFFRLR"))
      (check-false (valid-ticket? *test-ap* "FBFBBFRLR"))
      (check-false (valid-ticket? *test-ap* "FBFBBFFRLRR"))
      (check-false (valid-ticket? *test-ap* "FBFRBFFRLR"))
      (check-false (valid-ticket? *test-ap* "FBFBBFFRBR"))))

   (test-case
    "Check ticket to seat"
    (let* ([*test-ap* (make-airplane 128 8)] [seat (ticket->seat *test-ap* "FBFBBFFRLR")])
      (check-eq? (seat-row seat) 44)
      (check-eq? (seat-col seat) 5)
      (check-eq? (seat-num seat) 357)))


   (test-case
    "Check seat to ticket"
    (let ([*test-ap* (make-airplane 128 8)])
    (check-equal? (seat->ticket *test-ap* (make-seat *test-ap* 44 5)) "FBFBBFFRLR")
    (check-equal? (seat->ticket *test-ap* (make-seat *test-ap* 70 7)) "BFFFBBFRRR")
    (check-equal? (seat->ticket *test-ap* (make-seat *test-ap* 14 7)) "FFFBBBFRRR")
    (check-equal? (seat->ticket *test-ap* (make-seat *test-ap* 102 4)) "BBFFBBFRLL")))))
