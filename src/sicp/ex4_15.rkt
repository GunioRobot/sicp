#lang racket

(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

#|

Now, if you run (try try) then, we check if (try try) would halt. If it halts, then we run forever, else we return.
This is contradictory to the assumption.

|#