#lang racket/base

(require racket/hash racket/list racket/match racket/set racket/string)
(require "./data.rkt")

(provide load-bag-data find-all-containing find-all-contained-within)

; ------------------------------------------------------------------------------------------

(define (load-bag-data file-name)
  (load-data-from file-name bag-decode))

(define (bag-decode line)
  (match-let* ([(list outer inner) (string-split (string-replace line #px"\\s+bag(s?)\\s*\\.?" "") "contain" #:trim? #t)]
               [(list inner ...) (string-split inner "," #:trim? #t #:repeat? #t)])
    (cons
     outer
     (map (λ (bag)
            (let ([parsed (regexp-match #px"^\\s*(\\d+)\\s+(.+)$" bag)])
              (cond
                [(list? parsed)
                 (list (string->number (second parsed)) (third parsed))]
                [(string=? bag " no other") '()]
                [else parsed])))
          inner))))

(define (find-all-containing bag-data bag)
  (let find-more ([bag-hash (pivot-bag-data bag-data)]
                  [find-bag bag]
                  [found (set)])
    (let* ([next (list->set (hash-ref bag-hash find-bag '()))]
           [new (set-subtract next found)])
      (if (= (set-count new) 0)
          found
          (apply set-union found (set-map new (λ (b) (find-more bag-hash b (set-union new found)))))))))

(define (find-all-contained-within bag-data bag)
  (let find-more ([bag-hash (make-immutable-hash bag-data)]
                  [find-bag bag])
    (let* ([next (hash-ref bag-hash find-bag '())])
      (foldr + 0
             (map (λ (pair) (if (empty? pair)
                                0
                                (+ (first pair) (* (first pair) (find-more bag-hash (second pair))))))
                  next)))))

(define (pivot-bag-data bag-data)
  (let ([pivoted (make-hash)])
    (for ([bag bag-data])
      (let* ([outer (first bag)] [inners (list-tail bag 1)])
        (unless (empty? (first inners))
          (let ([inner-hash (make-immutable-hash (map (λ (pair) (cons (second pair) (list outer))) inners))])
            (hash-union! pivoted
                         inner-hash
                         #:combine/key (λ (k v1 v2) (append v1 v2)))))))
    pivoted))
  
; ------------------------------------------------------------------------------------------

(require rackunit)

(define bags-tests
  (test-suite
   "Bags test suite"
   
   (test-case
    "Decode a single line"
    (check-equal?
     (bag-decode "plaid magenta bags contain 2 clear lavender bags, 3 clear teal bags, 4 vibrant gold bags.")
     '("plaid magenta" (2 "clear lavender") (3 "clear teal") (4 "vibrant gold")))
    (check-equal?
     (bag-decode "dotted lavender bags contain no other bags.")
     '("dotted lavender" ())))

   (test-case
    "pivot"
    (check-equal? (hash->list (pivot-bag-data '(("foo" (2 "bar") (1 "baz")) ("baz" (1 "bar")))))
                  '(("bar" . ("foo" "baz")) ("baz" . ("foo")))))))
