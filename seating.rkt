#lang racket/base

(require racket/bool racket/function racket/list)
(require "./data.rkt")

(provide load-seat-map find-stable-seating seating-pass-p1 seating-pass-p2)

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
  (displayln ""))

(define (display-two-seat-maps pre post)
  (for ([r (range (seat-map-rows pre))])
    (display (vector-ref (seat-map-data pre) r))
    (display " -> ")
    (displayln (vector-ref (seat-map-data post) r)))
  (displayln ""))

(define (seating-pass-p1 data)
  (seat-map (for/vector ([row (in-range (seat-map-rows data))])
              (list->string
               (for/list ([col (in-range (seat-map-cols data))])
                 (cond
                   [(floor? data row col) #\.]
                   [(and (seat-empty? data row col)
                         (zero? (occupied-count data row col))) #\#]
                   [(and (seat-occupied? data row col)
                         (>= (occupied-count data row col) *too-many-neighbors*)) #\L]
                   [(string-ref (vector-ref (seat-map-data data) row) col)]))))
            (seat-map-rows data)
            (seat-map-cols data)))

(define (seating-pass-p2 data)
  (seat-map (for/vector ([row (in-range (seat-map-rows data))])
              (list->string
               (for/list ([col (in-range (seat-map-cols data))])
                 (cond
                   [(floor? data row col) #\.]
                   [(and (seat-empty? data row col)
                         (zero? (occupied-count data row col))) #\#]
                   [(and (seat-occupied? data row col)
                         (>= (occupied-count data row col) *too-many-neighbors*)) #\L]
                   [(string-ref (vector-ref (seat-map-data data) row) col)]))))
            (seat-map-rows data)
            (seat-map-cols data)))
     
(define (occupied-count data row col)
  (let ([this (list row col)] [coords (cartesian-product (range (sub1 row) (+ row 2)) (range (sub1 col) (+ col 2)))])
    (count (λ (coord)
             (and (not (equal? coord this))
                  (seat-occupied? data (first coord) (second coord))))
           (filter (curry valid-coord? data) coords))))

(define (find-stable-seating map seating-pass [trace? #f])
  (let next-iteration ([curr-map map])
    (let ([next-map (seating-pass curr-map)])
      (when trace? (display-two-seat-maps curr-map next-map))
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
     (check-equal? (find-stable-seating smap seating-pass-p1 #t) 37)
     (displayln (find-stable-seating smap seating-pass-p2 #t))))))
