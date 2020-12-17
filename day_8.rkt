#lang racket/base

(require racket/bool racket/list racket/set)

(require "aoc.rkt" "boot-code.rkt")

(define memory (load-and-assemble-code "day_8_input.txt"))

(define (make-break-on-loop)
  (let ([seen-pc (mutable-set)])
    (λ (pc inst oper acc)
      (cond
        [(set-member? seen-pc pc)
         #f]
        [else
         (set-add! seen-pc pc)
         #t]))))

(answer '(8 . 1)
        (cdr (execute memory #:break (make-break-on-loop)))
        1331)

(define (make-break-on-nop-jmp end jmps)
  (let ([seen-pc (mutable-set)])
    (λ (pc inst oper acc)
      (cond
        [(= inst *nop*)
         (if (> (+ pc oper) end)
             #f
             #t)]
        [(= inst *jmp*)
         (set-add! jmps (list pc acc))])
      (cond
        [(set-member? seen-pc pc)
         #f]
        [else
         (set-add! seen-pc pc)
         #t]))))

(define memory-size (vector-length memory))
(answer '(8 . 2)
        (let* ([jumps (mutable-set)]
               [nop->jmp (execute memory
                                  #:break (make-break-on-nop-jmp (vector-length memory) jumps))])
          (if (not (false? (car nop->jmp)))
              (cdr nop->jmp)
              (let ([jmp->nop (set-map jumps (λ (pc-acc)
                                               (list (execute memory
                                                              #:break (make-break-on-loop)
                                                              #:start (+ (first pc-acc)
                                                                         *instruction-size*)
                                                              #:init-acc (second pc-acc))
                                                     pc-acc)))])
                (cdr (first (first (filter (λ (result) (not (false? (car (first result))))) jmp->nop)))))))
        1121)


