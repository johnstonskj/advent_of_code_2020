#lang racket/base

(require racket/function racket/stream racket/vector)

(require "./data.rkt")

(provide load-cypher-text load-and-decode-cypher-text cypher-decode)

; ------------------------------------------------------------------------------------------

(define *window-size* 25)

(define-struct window-stream (start width step length)
  #:methods gen:stream
  [(define (stream-empty? stream)
     (= (window-stream-start stream)
        (- (window-stream-length stream) (window-stream-width stream) (window-stream-step stream))))
   (define (stream-first stream)
     (let* ([start (window-stream-start stream)]
            [end (+ start (window-stream-width stream))])
       (cons start end)))
   (define (stream-rest stream)
     (window-stream (+ (window-stream-start stream) (window-stream-step stream))
                    (window-stream-width stream)
                    (window-stream-step stream)
                    (window-stream-length stream)))])

(define (new-window-stream vec [window *window-size*] #:step [step 1])
  (if (< window (vector-length vec))
      (window-stream 0 (- window step) step (vector-length vec))
      #f))

; ------------------------------------------------------------------------------------------

(define (load-cypher-text file-name)
  (list->vector (load-data-from file-name string->number)))

(define (load-and-decode-cypher-text file-name)
  (cypher-decode (load-cypher-text file-name)))

(define (cypher-decode cypher-text #:window-width [window-width *window-size*])
  (if (< (vector-length cypher-text) window-width)
      #f
      (let ([stream (new-window-stream cypher-text window-width)])
        (for/list ([w stream])
          (let ([vec (vector-take (vector-drop cypher-text (car w)) window-width)]
                [sum (vector-ref cypher-text (add1 (cdr w)))])
            (cons sum (has-sum? vec sum)))))))

(define (has-sum? seq sum)
  (for*/or ([a seq] #:when (< a sum) [b seq] #:when (< b sum))
    (= (+ a b) sum)))

; ------------------------------------------------------------------------------------------

(require racket/bool racket/list rackunit)

(define xmas-cypher-tests
  (test-suite
   "XMAS Cypher test suite"

   (test-case
    "Check sliding window"
    (let* ([test-data '#(35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576)]
           [results (stream->list (new-window-stream test-data 5))])
      (check-equal? results
                    '((0 . 4) (1 . 5) (2 . 6) (3 . 7) (4 . 8) (5 . 9) (6 . 10) (7 . 11)
                              (8 . 12) (9 . 13) (10 . 14) (11 . 15) (12 . 16) (13 . 17)
                              (14 . 18)))))
   (test-case
    "Check has-sum"
    (check-true (has-sum? '(1 2 3 6 7) 5))
    (check-false (has-sum? '(1 2 3 6 7) 99)))

   (test-case
    "Check decode"
    (let* ([test-data '#(35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576)]
           [result (cypher-decode test-data #:window-width 5)]
           [failure (first (filter (λ (p) (false? (cdr p))) result))])
      (check-equal? (car failure) 127)))))



