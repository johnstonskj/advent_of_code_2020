#lang racket/base

(require racket/bool racket/stream rackunit)

(provide answer for/run)

; ------------------------------------------------------------------------------------------

(define *no-expected* (gensym))

(define (answer day-part value [expected *no-expected*] #:msg [msg #f])
  (let ([response (format "Day ~a, part ~a~a: ~a"
                          (car day-part)
                          (cdr day-part)
                          (if (string? msg)
                              (format " (~a)" msg)
                              (string))
                          value)])
    (cond
      [(eq? expected *no-expected*)
       (display response)
       (displayln " ?")]
      [(and (not (eq? expected *no-expected*))
            (equal? value expected))
       (display response)
       (displayln " is correct.")]
      [else
       (fail (format "value does '~a' not equal expected '~a'" value expected))])))

(define (for/run [days #f])
  (cond
    [(or (list? days) (stream? days))
     (for ([day days])
       (dynamic-require (format "day_~a.rkt" day) #f)
       (newline))]
    [(exact-nonnegative-integer? days)
     (for/run (in-range 1 (add1 days)))]
    [(false? days)
     (for/run (date-day (seconds->date (current-seconds))))]
    [else
     (fail (format "Invalid input value '~a'" days))]))