#lang racket/base

(require racket/list racket/string)
(require "./data.rkt")

(provide load-passport-data passport-valid?)

(define *field-sep* " ")

(define *key-sep* ":")

(define *optional* "cid")

(define *all-required-keys* "byr-ecl-eyr-hcl-hgt-iyr-pid")

(define (load-passport-data file-name)
  (load-chunked-data-from "day_4_input.txt" (λ (s) s)))

(define (passport-valid? passport)
  (string=? (passport-field-keys (passport-fields passport)) *all-required-keys*))

(define (passport-fields line)
  (string-split line *field-sep* #:trim? #t #:repeat? #t))

(define (passport-field-keys fields #:remove-optional [remove-optional? #t])
  (let ([keys (map (λ (s) (first (string-split s *key-sep*))) fields)])
    (string-join (sort (if remove-optional?
                           (filter-not (λ (s) (string=? s *optional*)) keys)
                           keys)
                       string<?) "-")))


  