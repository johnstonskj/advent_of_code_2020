#lang racket/base

(require "./passports.rkt")

(define passport-data (load-passport-data "./day_4_input.txt"))

(length (filter passport-valid? passport-data))
