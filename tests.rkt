#lang racket/base

(require rackunit rackunit/text-ui)

(require/expose "airplane.rkt" (airplane-tests))
(run-tests airplane-tests)

(require/expose "bags.rkt" (bags-tests))
(run-tests bags-tests)

(require/expose "boot-code.rkt" (boot-code-tests))
(run-tests boot-code-tests)

(require/expose "bus-schedule.rkt" (bus-tests))
(run-tests bus-tests)

(require/expose "customs.rkt" (customs-tests))
(run-tests customs-tests)

(require/expose "navigation.rkt" (navigation-tests))
(run-tests navigation-tests)

(require/expose "seating.rkt" (seating-tests))
(run-tests seating-tests)

(require/expose "xmas-cypher.rkt" (xmas-cypher-tests))
(run-tests xmas-cypher-tests)
