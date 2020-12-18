#lang racket/base

(require racket/bool racket/list racket/string)

(require "./data.rkt")

(provide load-pcs-initialization initialize-memory)

; ------------------------------------------------------------------------------------------

(struct instr
  (mask mem) #:transparent)

(define *line-regex* #px"^((mask)\\s+=\\s+([01X]+))|((mem)\\[(\\d+)\\]\\s+=\\s+([0-9]+))$")

(define (load-pcs-initialization file-name)
  (for/fold ([instrs '()]
             [mask #f]
             [mems '()]
             #:result (reverse (list* (instr mask (reverse mems)) instrs)))
            ([line (load-data-from file-name)])
    (let ([parsed (regexp-match *line-regex* line)])
      (when (list? parsed)
        (cond
          [(and (not (false? (third parsed))) (string=? (third parsed) "mask"))
           (if (false? mask)
               (values instrs (fourth parsed) mems)
               (values (list* (instr mask (reverse mems)) instrs)  (fourth parsed) '()))]
          [(string=? (sixth parsed) "mem")
           (values instrs
                   mask
                   (list* (cons (string->number (seventh parsed))
                                (string->number (eighth parsed)))
                          mems))])))))

(define (make-masks mask-string)
  (cons
   (string->number (string-replace mask-string "X" "0") 2)
   (string->number (string-replace mask-string "X" "1") 2)))

(define (mask-number masks num)
  (bitwise-and (bitwise-ior num (car masks)) (cdr masks)))

(define (initialize-memory instrs)
  (let ([results (make-hash)])
    (for ([instr instrs])
      (let ([masks (make-masks (instr-mask instr))])
        (for ([mem (instr-mem instr)])
          (displayln (format "~a = ~a (~a)" (car mem) (mask-number masks (cdr mem)) (cdr mem)))
          (hash-set! results (car mem) (mask-number masks (cdr mem))))))
    results))

; ------------------------------------------------------------------------------------------

(require racket/bool rackunit)

(define pcs-tests
  (test-suite
   "Port Computer System test suite"

   (test-case
    "find masking"
    (let ([masks (make-masks "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")])
      (check-equal? (mask-number masks 11) 73)
      (check-equal? (mask-number masks 101) 101)
      (check-equal? (mask-number masks 0) 64)))

   (test-case
    "memory init (overwrite)"
    (let ([results (initialize-memory (list (instr "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                                                   (list (cons 8 11) (cons 7 101) (cons 8 0)))))])
      (check-true (hash? results))
      (check-equal? (hash-count results) 2)
      (check-equal? (hash-ref results 7) 101)
      (check-equal? (hash-ref results 8) 64)
      (check-equal? (apply + (hash-values results)) 165)))))

