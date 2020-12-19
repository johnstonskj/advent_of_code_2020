#lang racket/base

(require racket/bool racket/format racket/list racket/string)

(require "./data.rkt")

(provide load-pcs-initialization initialize-memory-v1 initialize-memory-v2)

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

(define (initialize-memory-v1 instrs)
  (let ([results (make-hash)])
    (for* ([instr instrs]
           [mem (instr-mem instr)])
      (hash-set! results (car mem) (mask-number (make-masks (instr-mask instr)) (cdr mem))))
    results))

(define (initialize-memory-v2 instrs)
  (let ([results (make-hash)])
    (for* ([instr instrs]
           [mem (instr-mem instr)]
           [addr (all-addresses (car mem) (instr-mask instr))])
      (hash-set! results addr (cdr mem)))
    results))

(define (all-addresses address mask)
  (let* ([1-masked (bitwise-ior address (string->number (string-replace mask "X" "0") 2))]
         [indexes (remove '() (combinations (indexes-of (string->list mask) #\X)))]
         [masks (for/list ([x indexes])
                  (apply + (map (λ (b) (arithmetic-shift 1 (- 35 b))) x)))])
    (remove-duplicates
     (flatten
      (for/list ([m masks])
        (list (bitwise-ior 1-masked m) (bitwise-xor 1-masked m)))))))

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
    "memory init (v1)"
    (let ([results (initialize-memory-v1 (list (instr "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                                                      (list (cons 8 11) (cons 7 101) (cons 8 0)))))])
      (check-true (hash? results))
      (check-equal? (hash-count results) 2)
      (check-equal? (hash-ref results 7) 101)
      (check-equal? (hash-ref results 8) 64)
      (check-equal? (apply + (hash-values results)) 165)))

   (test-case
    "expand addresses"
    (check-equal? (all-addresses 42 "000000000000000000000000000000X1001X")
                  '(58 26 59 27))
    (check-equal? (all-addresses 26 "00000000000000000000000000000000X0XX")
                  '(26 18 24 16 27 19 25 17)))

   (test-case
    "memory init (v2)"
    (let ([results (initialize-memory-v2 (list (instr "000000000000000000000000000000X1001X"
                                                      (list (cons 42 100)))
                                               (instr "00000000000000000000000000000000X0XX"
                                                      (list (cons 26 1)))))])
      (check-true (hash? results))
      (check-equal? (hash-count results) 10)
      (check-equal? (hash-ref results 16) 1)
      (check-equal? (hash-ref results 59) 100)
      (check-equal? (apply + (hash-values results)) 208)))))
