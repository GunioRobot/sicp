#lang racket

(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(provide square)    