#lang racket/base

(require racket/list racket/string)

(require "./data.rkt")

(provide load-bus-data find-first-bus)

; ------------------------------------------------------------------------------------------

(define (load-bus-data file-name)
  (let ([lines (load-data-from file-name)])
    (cons (string->number (first lines))
          (filter-map
           (λ (s) (if (string=? s "x") #f (string->number s)))
           (string-split (second lines) ",")))))

(define (find-first-bus bus-data)
  (let ([min-wait (first (sort (map (λ (x) (cons (- x (modulo (car bus-data) x)) x)) (cdr bus-data))
                               #:key car <))])
    (* (car min-wait) (cdr min-wait))))

; ------------------------------------------------------------------------------------------

(require rackunit)


(define bus-tests
  (test-suite
   "Bus schedule test suite"

   (test-case
    "find first"  
    (check-equal? (find-first-bus '(939 . (7 13 59 31 19))) 295))))