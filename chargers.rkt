#lang racket/base

(require racket/list)

(require "./data.rkt")

(provide load-charger-jolts jolt-diffs charger-paths)

; ------------------------------------------------------------------------------------------

(define (load-charger-jolts file-name [final 3])
  (let ([input (sort (load-data-from file-name string->number) <)])
    (flatten (list 0 input (+ (last input) final)))))
    
(define (jolt-diffs jolts)
  (map - (drop jolts 1) (drop-right jolts 1)))

(define t '(0 1 2 3 4 5))
(define d (jolt-diffs t))

(define (charger-paths diffs)
  (apply *
         (for/fold ([count 0] [result '()] #:result result)
                   ([v diffs])
           (cond
             [(= 1 v) (values (add1 count) result)]
             [(= 0 count) (values count result)]
             [else (values 0 (cons (add1 (apply + (range count))) result))]))))

;(charger-paths '(1 1 1 3 1 1 3 1))