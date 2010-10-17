#lang racket

(require "utils.rkt")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x this-coeff) higher-terms))
              0
              coefficient-sequence))

(horner-eval 2 '(1 3 0 5 0 1))