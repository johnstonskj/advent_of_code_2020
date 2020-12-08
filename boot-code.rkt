#lang racket/base

(require racket/bool racket/format racket/function racket/list racket/match)
(require "./data.rkt")

(provide load-code load-and-assemble-code assemble-code dump disassemble execute log-tracer)

; ------------------------------------------------------------------------------------------

(define *instruction-size* 2)

(define *nop* 0)
(define *acc* 1)
(define *jmp* 2)

(define *err* 255)

; ------------------------------------------------------------------------------------------

(define (load-code file-name)
  (load-data-from file-name identity))
                       
(define (load-and-assemble-code file-name)
  (assemble-code (load-code file-name)))

(define (assemble-code code)
  (list->vector (flatten (map (λ (line) (assemble-line line)) code))))

(define (assemble-line line)
  (let ([parsed  (regexp-match #px"^\\s*([a-z]+)\\s+([+-]\\d+)\\s*$" line)])
    (if (list? parsed)
        (list (match (second parsed)
                ["nop" *nop*]
                ["acc" *acc*]
                ["jmp" *jmp*]
                [else *err*])
              (string->number (third parsed)))
        (list *err* 0))))
  
(define (disassemble memory #:start [start 0])
  (if (and (vector? memory) (< start (vector-length memory)))
      (let next-instruction ([curr start] [end (vector-length memory)])
        (if (< curr end)
            (let ([instruction (vector-ref memory curr)] [operand (vector-ref memory (+ curr 1))])
              (log-tracer curr instruction operand)
              (next-instruction (+ curr *instruction-size*) end))
            #t))
      #f))

(define (disassemble-instruction instruction)
  (cond
    [(= instruction *nop*) "nop"]
    [(= instruction *acc*) "acc"]
    [(= instruction *jmp*)"jmp"]
    [else (display "ERR")]))

; (tracefn pc instruction operand)
(define (execute memory
                 #:start [start 0] #:trace [tracefn #f] #:break [breakfn #f] #:limit [limit #f])
  (if (and (vector? memory) (< start (vector-length memory)))
      (let next-instruction ([pc start] [acc 0] [end (vector-length memory)] [count 0])
        (if (and (< pc end) (or (false? limit) (and (number? limit) (< count limit))))
            (let ([instruction (vector-ref memory pc)]
                  [operand (vector-ref memory (+ pc 1))])
              (when (procedure? tracefn)
                (tracefn pc instruction operand (~r acc)))
              (if (or (false? breakfn) (and (procedure? breakfn) (breakfn pc instruction operand)))
                (apply next-instruction
                       (append
                        (cond
                          [(= instruction *nop*) (list (+ pc *instruction-size*) acc end)]
                          [(= instruction *acc*) (list (+ pc *instruction-size*) (+ acc operand) end)]
                          [(= instruction *jmp*) (list (+ pc (* operand *instruction-size*)) acc end)]
                          [else (displayln "PANIC")
                                (list end acc end)])
                        (list (+ count 1))))
              acc))
            acc))
      #f))

(define (dump memory)
  (if (vector? memory)
      (let next-row ([row-start 0] [end (vector-length memory)])
        (if (< row-start end)
            (let* ([row-len (* *instruction-size* 8)]
                   [row-end? (+ row-start row-len)]
                   [row-end (if (< row-end? end) row-end? end)])
              (display (~r row-start #:base 16 #:min-width 4 #:pad-string "0"))
              (display ": ")
              (for ([i (in-range row-start row-end *instruction-size*)])
                (display (~r (vector-ref memory i)  #:base 16 #:min-width 2 #:pad-string "0"))
                (display " ")
                (display (~r (vector-ref memory (+ i 1))  #:base 16 #:min-width 2 #:pad-string "0"))
                (display " "))
              (displayln "")
              (next-row row-end end))
            #t))
      #f))

(define (log-tracer pc instruction operand [comment #f])
  (display (~r pc #:base 16 #:min-width 4 #:pad-string "0"))
  (display ": ")
  (display (disassemble-instruction instruction))
  (display " ")
  (display (~r operand #:sign '++))
  (when comment
    (display " ; ")
    (display comment))
  (displayln "")
  #t)

; ------------------------------------------------------------------------------------------

(require rackunit)

(test-case
 "assemble single line"
 (check-equal? (assemble-line "acc +2") '(1 2)))

;(test-case
; "Check execute"
; (check-equal?
;  (execute
;   (assemble-code
;    '("nop +0" "acc +1" "jmp +4" "acc +3" "jmp -3" "acc -99" "acc +1" "jmp -4" "acc +6"))
;   #:limit 20)
;  16))