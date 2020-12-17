#lang racket/base

(require racket/bool)

(require "aoc.rkt" "passwords.rkt")

(define (counting-policy-evaluate p password)
  (let* ([chars (filter (λ (c) (char=? c (policy-character p))) (string->list password))]
         [number (string-length (list->string chars))])
    (and (>= number (policy-min p))
         (<= number (policy-max p)))))

(define (positional-policy-evaluate p password)
  (let* ([chars (list->vector (string->list password))]
         [first (vector-ref chars (- (policy-min p) 1))]
         [second (vector-ref chars (- (policy-max p) 1))]
         [policy-char (policy-character p)])
    (xor (char=? first policy-char)
         (char=? second policy-char))))

(define password-values (load-password-values "day_2_input.txt"))

(answer '(2 . 1) (policy-evaluate-all counting-policy-evaluate password-values) 393)

(answer '(2 . 2) (policy-evaluate-all positional-policy-evaluate password-values) 690)