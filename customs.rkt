#lang racket/base

(require racket/set racket/string)
(require "./data.rkt")

(provide load-customs-data group-unique group-union-count group-intersect-count
         group->string group->list group->lists)

; ------------------------------------------------------------------------------------------

(define (load-customs-data file-name)
  (load-chunked-data-from file-name (λ (s) (string-split s " " #:trim? #t #:repeat? #t))))

(define (group->string g)
  (string-join g ""))

(define (group->list g)
  (string->list (string-join g "")))

(define (group->lists g)
  (map string->list g))

(define (group-unique g)
  (set->list (list->set (group->list g))))

(define (group-union-count g)
  (set-count (list->set (group->list g))))

(define (group-intersect-count g)
  (set-count (apply set-intersect (map list->set (group->lists g)))))

; ------------------------------------------------------------------------------------------

(require rackunit)

(define customs-tests
  (test-suite
   "Customs test suite"
  
   (test-case
    "Check union count"
    (check-equal? (group-union-count '("abc")) 3)
    (check-equal? (group-union-count '("a" "b" "c")) 3)
    (check-equal? (group-union-count '("ab" "bc")) 3)
    (check-equal? (group-union-count '("a" "a" "a" "a")) 1)
    (check-equal? (group-union-count '("b")) 1))


   (test-case
    "Check intersect count"
    (check-equal? (group-intersect-count '("abc")) 3)
    (check-equal? (group-intersect-count '("a" "b" "c")) 0)
    (check-equal? (group-intersect-count '("ab" "bc")) 1)
    (check-equal? (group-intersect-count '("a" "a" "a" "a")) 1)
    (check-equal? (group-intersect-count '("b")) 1))))
