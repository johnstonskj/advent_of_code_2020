#lang racket/base

(require racket/bool racket/list racket/stream)

(provide play-memory-game)

; ------------------------------------------------------------------------------------------

(define-struct memory-game (history turn end)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? stream)
     (or (empty? (memory-game-history stream))
         (= (length (memory-game-history stream)) (memory-game-end stream))))
   (define (stream-first stream)
     (first (memory-game-history stream)))
   (define (stream-rest stream)
     (memory-game (new-history (memory-game-history stream) (memory-game-turn stream))
                  (add1 (memory-game-turn stream))
                  (memory-game-end stream)))
   ])

(define (play-memory-game initial-history last-turn)
  (for/last ([v (memory-game (new-history (reverse initial-history) (length initial-history))
                             (length initial-history)
                             (add1 last-turn))])
    v))

(define (new-history history cturn)
  (let* ([last (first history)]
         [index (index-of (rest history) last)])
    (if (false? index)
        (list* 0 history)
        (let ([turn (- cturn (add1 index))])
          (list* (- cturn turn) history)))))


; ------------------------------------------------------------------------------------------

(require rackunit)

(check-equal? (play-memory-game '(0 3 6) 2020) 436)

(check-equal? (play-memory-game '(0 3 6) 30000000) 175594)