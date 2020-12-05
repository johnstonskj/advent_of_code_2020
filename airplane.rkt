#lang racket/base

(require racket/file racket/list)

(provide load-ticket-data make-airplane airplane-rows airplane-cols make-seat seat-col seat-row seat-num valid-ticket? ticket->seat)

; ------------------------------------------------------------------------------------------

(define (load-ticket-data file-name)
  (file->lines file-name))

; ------------------------------------------------------------------------------------------

(struct airplane
  (rows rdiv cols cdiv) #:transparent)

(define *invalid-configuration* 'INVALID-AIRPLANE-CONFIGURATION)

(define (make-airplane rows cols)
  (if (and (> rows 0)
           (< rows 500)
           (> cols 0)
           (< cols 25))
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

(define *invalid-ticket* 'INVALID-TICKET)

(define (valid-ticket? ap ticket)
  (let ([chars (string->list ticket)])
    (and
     (= (length chars) (+ (airplane-rdiv ap) (airplane-cdiv ap))) 
     (for/and ([c (take chars (airplane-rdiv ap))]) (or (char=? c #\B) (char=? c #\F)))
     (for/and ([c (drop chars (airplane-rdiv ap))]) (or (char=? c #\L) (char=? c #\R))))))

(define (ticket->seat ap ticket)
  (if (not (valid-ticket? ap ticket))
      *invalid-ticket*
      (let*  ([chars (string->list ticket)]
              [rows (take chars (airplane-rdiv ap))]
              [cols (drop chars (airplane-rdiv ap))])
      (make-seat ap
                 (bsp-find rows #\F #\B 0 (- (airplane-rows ap) 1))
                 (bsp-find cols #\L #\R 0 (- (airplane-cols ap) 1))))))

(define (bsp-find lst low high min max)
  (if (= (length lst) 1)
      (if (char=? (first lst) low)
          min
          max)
      (if (char=? (first lst) low)
          (bsp-find (drop lst 1) low high min (inexact->exact (+ min (floor (/ (- max min) 2)))))
          (bsp-find (drop lst 1) low high (inexact->exact (+ min (ceiling (/ (- max min) 2)))) max))))

; ------------------------------------------------------------------------------------------

(require rackunit)

(test-case
 "Check make airplane"
 (let ([ap (make-airplane 128 8)])
   (check-eq? (airplane-rows ap) 128)
   (check-eq? (airplane-rdiv ap) 7)
   (check-eq? (airplane-cols ap) 8)
   (check-eq? (airplane-cdiv ap) 3)))

(define *test-ap* (make-airplane 128 8))

(test-case
 "Check ticket valid"
 (check-true (valid-ticket? *test-ap* "FBFBBFFRLR"))
 (check-false (valid-ticket? *test-ap* "FBFBBFRLR"))
 (check-false (valid-ticket? *test-ap* "FBFBBFFRLRR"))
 (check-false (valid-ticket? *test-ap* "FBFRBFFRLR"))
 (check-false (valid-ticket? *test-ap* "FBFBBFFRBR")))

(test-case
 "Check ticket to seat"
 (let ([seat (ticket->seat *test-ap* "FBFBBFFRLR")])
   (check-eq? (seat-row seat) 44)
   (check-eq? (seat-col seat) 5)
   (check-eq? (seat-num seat) 357)))
