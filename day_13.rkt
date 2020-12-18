#lang racket/base

(require "aoc.rkt" "bus-schedule.rkt")

(define bus-data (load-bus-data "day_13_input.txt"))

(answer '(13 . 1) (find-first-bus bus-data) 2165)

(answer '(13 . 2) (find-lowest-common-start bus-data) 534035653563227)
