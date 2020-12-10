#lang racket/base

(require rackunit rackunit/text-ui)

(require/expose "airplane.rkt" (airplane-tests))
(run-tests airplane-tests)

(require/expose "bags.rkt" (bags-tests))
(run-tests bags-tests)

(require/expose "boot-code.rkt" (boot-code-tests))
(run-tests boot-code-tests)

(require/expose "customs.rkt" (customs-tests))
(run-tests customs-tests)

(require/expose "xmas-cypher.rkt" (xmas-cypher-tests))
(run-tests xmas-cypher-tests)
