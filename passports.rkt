#lang racket/base

(require racket/list racket/string)
(require "./data.rkt")

(provide load-passport-data passport-has-required-keys? passport-valid?)

; ------------------------------------------------------------------------------------------

(define *field-sep* " ")

(define *key-sep* ":")

(define *optional* "cid")

(define *all-required-keys* "byr-ecl-eyr-hcl-hgt-iyr-pid")

; ------------------------------------------------------------------------------------------

(define (load-passport-data file-name)
  (load-chunked-data-from "day_4_input.txt" (λ (s) s)))

(define (passport-has-required-keys? passport)
  (string=? (passport-field-keys  (passport-fields passport)) *all-required-keys*))
  
(define (passport-valid? passport)
  (let ([fields (passport-fields passport)])
    (and
     ; Check all required fields present
     (string=? (passport-field-keys fields) *all-required-keys*)
     ; Check validation for each field
     (for/and ([field fields])
       (let ([key (first field)] [value (second field)])
         (cond
           ; (Birth Year) - four digits; at least 1920 and at most 2002.
           [(string=? key "byr")
            ; simplified: (>= 2002 (string->number value) 1920)
            (let ([year (string->number value)])
              (and (>= year 1920) (<= year 2002)))]
           ; (Issue Year) - four digits; at least 2010 and at most 2020.
           [(string=? key "iyr")
            (let ([year (string->number value)])
              (and (>= year 2010) (<= year 2020)))]
           ; (Expiration Year) - four digits; at least 2020 and at most 2030.
           [(string=? key "eyr")
            (let ([year (string->number value)])
              (and (>= year 2020) (<= year 2030)))]
           ; (Height) - a number followed by either cm or in:
           ;   - If cm, the number must be at least 150 and at most 193.
           ;   - If in, the number must be at least 59 and at most 76.
           [(string=? key "hgt")
            (let ([parsed (regexp-match #px"^(\\d{2,3})(cm|in)$" value)])
              (if (list? parsed)
                  (let ([v (string->number (second parsed))] [u (third parsed)])
                    (if (string=? u "cm")
                        (and (>= v 150) (<= v 193))
                        (and (>= v 59) (<= v 76))))
                  parsed))]
           ; (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
           [(string=? key "hcl")
            (list? (regexp-match #px"^#[0-9a-f]{6}$" value))]
           ;(Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
           [(string=? key "ecl")
            (list? (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))]
           ; pid (Passport ID) - a nine-digit number, including leading zeroes.
           [(string=? key "pid")
            (list? (regexp-match #px"^\\d{9}$" value))]
           ; (Country ID) - ignored, missing or not.
           [(string=? key "cid") #t]
           [else #f]))))))

(define (passport-fields line)
  (map (λ (s) (string-split s *key-sep*)) (string-split line *field-sep* #:trim? #t #:repeat? #t)))

(define (passport-field-keys fields #:remove-optional [remove-optional? #t])
  (let ([keys (map (λ (f) (first f)) fields)])
    (string-join (sort (if remove-optional?
                           (filter-not (λ (s) (string=? s *optional*)) keys)
                           keys)
                       string<?) "-")))


