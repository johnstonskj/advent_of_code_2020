#lang racket/base

(require racket/bool racket/list racket/string)

(require (only-in math/number-theory solve-chinese))

(require "./data.rkt")

(provide load-bus-data find-first-bus find-lowest-common-start)

; ------------------------------------------------------------------------------------------

(define (load-bus-data file-name)
  (let ([lines (load-data-from file-name)])
    (cons (string->number (first lines))
          (map
           (λ (s) (if (string=? s "x") #f (string->number s)))
           (string-split (second lines) ",")))))

(define (find-first-bus bus-data)
  (let ([min-wait (first (sort (filter-map (λ (x) (if (false? x)
                                                      x
                                                      (cons (- x (modulo (car bus-data) x)) x)))
                                           (cdr bus-data))
                               #:key car <))])
    (* (car min-wait) (cdr min-wait))))

(define (find-lowest-common-start bus-data)
  (let ([bus-offsets (for/list ([index (in-naturals)]
                                [bus (cdr bus-data)]
                                #:when (number? bus))
                       (cons index bus))])
    (let* ([residuals (map (λ (p) (- (cdr p) (car p))) bus-offsets)]
           [moduli (map cdr bus-offsets)]
           [result (solve-chinese residuals moduli)])
      result)))

; ------------------------------------------------------------------------------------------

(require rackunit)

(define bus-tests
  (test-suite
   "Bus schedule test suite"

   (test-case
    "find first"  
    (check-equal? (find-first-bus '(939 . (7 13 #f #f 59 #f 31 19))) 295))

   (test-case
    "find via chinese"
    (check-equal? (find-lowest-common-start '(0 . (17 #f 13 19))) 3417)
    (check-equal? (find-lowest-common-start '(0 . (67 7 59 61))) 754018)
    (check-equal? (find-lowest-common-start '(0 . (67 #f 7 59 61))) 779210)
    (check-equal? (find-lowest-common-start '(0 . (67 7 #f 59 61))) 1261476)
    (check-equal? (find-lowest-common-start '(0 . (1789 37 47 1889))) 1202161486))))
