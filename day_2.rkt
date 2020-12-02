#lang racket

(provide policy load-values policy-evaluate-all policy-evaluate)
(require "./data.rkt")

(struct policy
  (min max character)
  #:transparent)

(define (load-values file-name)
  (load-data-from file-name password-check-parse))

(define (policy-evaluate-all pairs)
  (length (filter (λ (pair) (policy-evaluate (first pair) (second pair))) pairs)))
  
(define (policy-evaluate p password)
  (let* ([chars (filter (λ (c) (char=? c (policy-character p))) (string->list password))]
         [number (string-length (list->string chars))])
    (and (>= number (policy-min p))
         (<= number (policy-max p)))))

(define (password-check-parse line)
  (let ([parsed (regexp-match #px"^(\\d+)\\-(\\d+) (.): (.+$)" line)])
    (cond
         [(and (list? parsed) (= (length parsed) 5))
          (list (policy (string->number (second parsed))
                        (string->number (third parsed))
                        (first (string->list (fourth parsed))))
                (fifth parsed))
          ]
         [else parsed])))

(policy-evaluate-all (load-values "day_2_input.txt"))