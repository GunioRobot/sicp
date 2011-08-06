#lang racket

(define (RC R C dt)
  (define (RC1 i initial-vc)
    (add-streams (integral (scale-stream i (/ 1.0 C)) initial-vc dt)
                 (scale-stream i R)))
  RC1)

(define RC1 (RC 5 1 0.5))