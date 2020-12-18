#lang racket/base

(require racket/bool racket/function racket/list)
(require "./data.rkt")

(provide load-seat-map
         find-stable-seating seating-pass count-occupied-neighbors count-occupied-visible)

; ------------------------------------------------------------------------------------------

(struct seat-map
  (data rows cols) #:transparent)

(define *too-many-neighbors* 4)

; ------------------------------------------------------------------------------------------

(define (load-seat-map file-name)
  (let ([data (list->vector (load-data-from file-name))])
    (seat-map data (vector-length data) (string-length (vector-ref data 0)))))

(define (seat-equal? map row col char)
  (char=? (string-ref (vector-ref (seat-map-data map) row) col) char))

(define (seat-empty? map row col)
  (seat-equal? map row col #\L))

(define (seat-occupied? map row col)
  (seat-equal? map row col #\#))

(define (floor? map row col)
  (seat-equal? map row col #\.))

(define (valid-coord? map coord)
  (let ([row (first coord)] [col (second coord)])
    (and
     (>= row 0) (< row (seat-map-rows map))
     (>= col 0) (< col (seat-map-cols map)))))

(define (display-seat-map map)
  (for ([r (seat-map-data map)]) (displayln r))
  (newline))

(define (display-two-seat-maps pre post)
  (for ([r (range (seat-map-rows pre))])
    (display (vector-ref (seat-map-data pre) r))
    (display " -> ")
    (displayln (vector-ref (seat-map-data post) r)))
  (newline))

(define (count-occupied-neighbors data row col)
  (let* ([this (list row col)]
         [coords (remove this (cartesian-product (range (sub1 row) (+ row 2)) (range (sub1 col) (+ col 2))))])
    (count (λ (coord)
             (and (not (equal? coord this))
                  (seat-occupied? data (first coord) (second coord))))
           (filter (curry valid-coord? data) coords))))

(define (count-occupied-visible data row col)
  (define (counter data r dr c dc)
    (cond
      [(not (valid-coord? data (list r c))) 0]
      [(seat-empty? data r c) 0]
      [(seat-occupied? data r c) 1]
      [else (counter data (+ r dr) dr (+ c dc) dc)]))
  (+ (counter data (sub1 row) -1 col 0)   ; up
     (counter data (add1 row) 1 col 0)    ; down
     (counter data (sub1 row) -1 (sub1 col) -1)   ; up left
     (counter data (sub1 row) -1 (add1 col) 1)   ; up right
     (counter data (add1 row) 1 (sub1 col) -1)    ; down left
     (counter data (add1 row) 1 (add1 col) 1)    ; down left
     (counter data row 0 (sub1 col) -1)   ; left
     (counter data row 0 (add1 col) 1)))  ; right

(define (seating-pass data occupied-counter neighbor-limit)
  (seat-map (for/vector ([row (in-range (seat-map-rows data))])
              (list->string
               (for/list ([col (in-range (seat-map-cols data))])
                 (cond
                   [(floor? data row col) #\.]
                   [(and (seat-empty? data row col)
                         (zero? (occupied-counter data row col))) #\#]
                   [(and (seat-occupied? data row col)
                        (>= (occupied-counter data row col) neighbor-limit)) #\L]
                   [(string-ref (vector-ref (seat-map-data data) row) col)]))))
            (seat-map-rows data)
            (seat-map-cols data)))

(define (find-stable-seating map occupied-counter neighbor-limit #:trace? [trace? #f])
  (let next-iteration ([curr-map map])
    (let ([next-map (seating-pass curr-map occupied-counter neighbor-limit)])
      (when trace?
        (display-two-seat-maps curr-map next-map))
      (if (equal? (seat-map-data curr-map) (seat-map-data next-map))
          (apply +
                 (for/list ([s (seat-map-data next-map)])
                   (count (curry char=? #\#) (string->list s))))
          (next-iteration next-map)))))

; ------------------------------------------------------------------------------------------

(require rackunit)

(define seating-tests
  (test-suite
   "Seat map test suite"

   (test-case
    "find end"  
    (let* ([data (list->vector '("L.LL.LL.LL"
                                 "LLLLLLL.LL"
                                 "L.L.L..L.."
                                 "LLLL.LL.LL"
                                 "L.LL.LL.LL"
                                 "L.LLLLL.LL"
                                 "..L.L....."
                                 "LLLLLLLLLL"
                                 "L.LLLLLL.L"
                                 "L.LLLLL.LL"))]
           [smap (seat-map data (vector-length data) (string-length (vector-ref data 0)))])
      (check-equal? (find-stable-seating smap count-occupied-neighbors 4 #:trace? #f) 37)
      (check-equal? (find-stable-seating smap count-occupied-visible 5 #:trace? #t) 26)))))
