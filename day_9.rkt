#lang racket

(require racket/vector)

(require "aoc.rkt" "xmas-cypher.rkt")

(define cypher-text (load-cypher-text "day_9_input.txt"))

(define failed-case (car (first (filter (λ (p) (false? (cdr p))) (cypher-decode cypher-text)))))
(answer '(9 . 1) failed-case 144381670 )

(define (find-sumands vec sum)
  (let ([length (vector-length vec)])
    (let outer ([i 0])
      (if (>= i length)
          #f
          (let ([result (let inner ([j (+ i 1)] [count 1] [csum (vector-ref vec i)])
                          (cond
                            [(and (> count 1) (= csum sum)) (vector-copy vec i j)]
                            [(> csum sum) #f]
                            [else (inner (+ j 1) (+ count 1) (+ csum (vector-ref vec j)))]
                            ))])
            (if (false? result)
                (outer (+ i 1))
                result))))))

(define failure-set (sort (vector->list (find-sumands cypher-text failed-case)) <))

(answer '(9 . 2) (+ (first failure-set) (last failure-set)) 20532569)
