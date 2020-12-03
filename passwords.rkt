#lang racket/base

(require "./data.rkt")

(provide (struct-out policy) load-password-values policy-evaluate-all)

(struct policy
  (min max character)
  #:transparent)

(define (load-password-values file-name)
  (load-data-from file-name password-check-parse))

(define (policy-evaluate-all policy-check pairs)
  (length (filter (λ (pair) (policy-check (first pair) (second pair))) pairs)))
  
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

