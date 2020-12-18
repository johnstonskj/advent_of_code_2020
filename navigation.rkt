#lang racket/base

(require racket/list racket/match)

(require "./data.rkt")

(provide load-navigation-instructions navigate manhattan-distance
         *north* *east* *south* *west*)

; ------------------------------------------------------------------------------------------

(define *north* 0)
(define *east* 90)
(define *south* 180)
(define *west* 270)

(struct position
  (lat long facing) #:transparent)

(define (load-navigation-instructions file-name)
  (load-data-from file-name parse-instruction))

(define (parse-instruction instr)
  (cons
   (string-ref instr 0)
   (string->number (substring instr 1))))

(define (manhattan-distance pos)
  (+ (abs (position-lat pos))
     (abs (position-long pos))))

(define (navigate instructions #:facing [facing 90])
  (manhattan-distance (foldl nav-step (position 0 0 facing) instructions)))

(define (nav-step instruction pos)
  (match (car instruction)
    [#\N
     (position (+ (position-lat pos) (cdr instruction))
               (position-long pos)
               (position-facing pos))]
    [#\S
     (position (- (position-lat pos) (cdr instruction))
               (position-long pos)
               (position-facing pos))]
    [#\E
     (position (position-lat pos)
               (+ (position-long pos) (cdr instruction))
               (position-facing pos))]
    [#\W
     (position (position-lat pos)
               (- (position-long pos) (cdr instruction))
               (position-facing pos))]
    [#\L
     (position (position-lat pos)
               (position-long pos) (rotate (position-facing pos)
                                           (- (cdr instruction))))]
    [#\R
     (position (position-lat pos)
               (position-long pos) (rotate (position-facing pos)
                                           (cdr instruction)))]
    [#\F
     (nav-step (cons (cond 
                       [(= (position-facing pos) *north*) #\N]
                       [(= (position-facing pos) *east*) #\E]
                       [(= (position-facing pos) *south*) #\S]
                       [(= (position-facing pos) *west*) #\W])
                     (cdr instruction))
               pos)
     ]))

(define (rotate from by)
  (let ([rotated (+ from by)])
    (cond
      [(= rotated 360) 0]
      [(> rotated 360) (- rotated 360)]
      [(< rotated 0) (- 360 (abs rotated))]
      [else rotated])))


(define (navigate/with-waypoint instructions waypoint #:facing [facing 90])
  (manhattan-distance (car (foldl nav-step/with-waypoint
         (cons (position 0 0 facing)
               (position (car waypoint) (cdr waypoint) 0))
         instructions))))

(define (nav-step/with-waypoint instruction pos)
  (let ([ship (car pos)] [waypoint (cdr pos)])
    (match (car instruction)
      [#\N
       (cons ship
             (position (+ (position-lat pos) (cdr instruction))
                       (position-long pos)
                       (position-facing pos)))]
      [#\S
       (cons ship
             (position (- (position-lat pos) (cdr instruction))
                       (position-long pos)
                       (position-facing pos)))]
      [#\E
       (cons ship
             (position (position-lat pos)
                       (+ (position-long pos) (cdr instruction))
                       (position-facing pos)))]
      [#\W
       (cons ship
             (position (position-lat pos)
                       (- (position-long pos) (cdr instruction))
                       (position-facing pos)))]
      [#\L
       (position (position-lat pos)
                 (position-long pos)
                 (rotate/around ship
                                (position-facing pos)
                                (- (cdr instruction))))]
      [#\R
       (position (position-lat pos)
                 (position-long pos)
                 (rotate/around ship
                                (position-facing pos)
                                (cdr instruction)))]
      [#\F
       (nav-step (cons (cond 
                         [(= (position-facing pos) *north*) #\N]
                         [(= (position-facing pos) *east*) #\E]
                         [(= (position-facing pos) *south*) #\S]
                         [(= (position-facing pos) *west*) #\W])
                       (cdr instruction))
                 pos)
       ])))

(define (rotate/around pos from by)
  0)

; ------------------------------------------------------------------------------------------

(require rackunit)

(define navigation-tests
  (test-suite
   "Navigation test suite"

   (test-case
    "find distance"  
    (check-equal?
     (navigate '((#\F . 10) (#\N . 3) (#\F . 7) (#\R . 90) (#\F . 11))) 25))

   (test-case
    "find distance with waypoint"  
    (check-equal?
     (car (navigate/with-waypoint '((#\F . 10) (#\N . 3) (#\F . 7) (#\R . 90) (#\F . 11))
                                  '(1 . 10))) 286))))

 
(car (navigate/with-waypoint '((#\F . 10) (#\N . 3) (#\F . 7) (#\R . 90) (#\F . 11))
                             '(1 . 10)))