#lang racket/base

(require racket/list racket/string)
(require "./data.rkt")

(define *field-sep* " ")

(define *key-sep* ":")

(define *optional* "cid")

(define (load-passport-data file-name)
  (load-chunked-data-from "day_4_input.txt" passport-fields))

(define (passport-fields line)
  (let ([fields (map (λ (s) (first (string-split s *key-sep*)))
                     (string-split line *field-sep* #:trim? #t #:repeat? #t))])
    (displayln fields)
    (string-join (sort  (filter-not (λ (s) (string=? s *key-sep*)) fields) string<?) "")))

(define passport-data (load-passport-data "./day_4_input.txt"))

