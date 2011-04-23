#lang racket

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(define (c+ a b)
  (let ((c (make-connector)))
    (adder a b c)
    c))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv v)
  (let ((z (make-connector)))
    (constant z v)
    z))
