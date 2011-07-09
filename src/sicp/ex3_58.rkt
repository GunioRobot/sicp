#lang racket

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

#|

The function gives the digits in the fractional part of the division as a
stream.

|#