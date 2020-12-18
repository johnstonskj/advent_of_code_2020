#lang racket/base

(require "aoc.rkt" "bus-schedule.rkt")

(define bus-data (load-bus-data "day_13_input.txt"))

(displayln bus-data)

(answer '(13 . 1) (find-first-bus bus-data))
