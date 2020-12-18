#lang racket/base

(require racket/list racket/match)

(require "./data.rkt")

(provide load-navigation-instructions navigate navigate/with-waypoint
         *facing-north* *facing-east* *facing-south* *facing-west*)

; ------------------------------------------------------------------------------------------

(define *facing-north* 0)
(define *facing-east* 90)
(define *facing-south* 180)
(define *facing-west* 270)

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

(define (navigate instructions #:facing [facing *facing-east*])
  (manhattan-distance (foldl nav-step (position 0 0 facing) instructions)))

(define (nav-step instruction ship)
  (match (car instruction)
    [#\N (move-position ship (position (cdr instruction) 0 0))]
    [#\S (move-position ship (position (- (cdr instruction)) 0 0))]
    [#\E (move-position ship (position 0 (cdr instruction) 0))]
    [#\W (move-position ship (position 0 (- (cdr instruction)) 0))]
    [#\R (move-position ship (position 0 0 (cdr instruction)))]
    [#\L (move-position ship (position 0 0 (- (cdr instruction))))]
    [#\F
     (nav-step (cons (cond 
                       [(= (position-facing ship) *facing-north*) #\N]
                       [(= (position-facing ship) *facing-east*) #\E]
                       [(= (position-facing ship) *facing-south*) #\S]
                       [(= (position-facing ship) *facing-west*) #\W])
                     (cdr instruction))
               ship)
     ]))

(define (position->string pos)
  (format "~a~a ~a~a ~a°"
                     (abs (position-lat pos))
                     (if (positive? (position-lat pos)) #\N #\S)
                     (abs (position-long pos))
                     (if (positive? (position-long pos)) #\E #\W)
                     (position-facing pos)))

(define (move-position pos by)
  (position (+ (position-lat pos) (position-lat by))
            (+ (position-long pos) (position-long by))
            (modulo (+ (position-facing pos) (position-facing by)) 360)))

(define (transpose-position pos by)
  (match (if (positive? by) by (+ 360 by))
    [(or 0 360) pos]
    [90 (position (- (position-long pos)) (position-lat pos) (position-facing pos))]
    [180 (position (- (position-lat pos)) (- (position-long pos)) (position-facing pos))]
    [270 (position (position-long pos) (- (position-lat pos)) (position-facing pos))]))

(define (navigate/with-waypoint instructions waypoint #:facing [facing *facing-east*])
  (manhattan-distance (car (foldl nav-step/with-waypoint
                                  (cons (position 0 0 facing)
                                        (position (car waypoint) (cdr waypoint) 0))
                                  instructions))))

(define (nav-step/with-waypoint instruction pos-pair)
  (let ([ship (car pos-pair)] [waypoint (cdr pos-pair)])
    (match (car instruction)
      [#\N (cons ship (move-position waypoint (position (cdr instruction) 0 0)))]
      [#\S (cons ship (move-position waypoint (position (- (cdr instruction)) 0 0)))]
      [#\E (cons ship (move-position waypoint (position 0 (cdr instruction) 0)))]
      [#\W (cons ship (move-position waypoint (position 0 (- (cdr instruction)) 0)))]
      [#\R (cons ship (transpose-position waypoint (cdr instruction)))]
      [#\L (cons ship (transpose-position waypoint (- (cdr instruction))))]
      [#\F (cons (position (+ (position-lat ship) (* (position-lat waypoint) (cdr instruction)))
                           (+ (position-long ship) (* (position-long waypoint) (cdr instruction)))
                           (position-facing ship))
                 waypoint)])))

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
     (navigate/with-waypoint '((#\F . 10) (#\N . 3) (#\F . 7) (#\R . 90) (#\F . 11))
                                  '(1 . 10))) 286)))
