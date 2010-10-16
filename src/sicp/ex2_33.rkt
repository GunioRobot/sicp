#lang racket

(require "utils.rkt")

(define (map f coll)
  (accumulate (lambda (x y) (cons (f x) y))
              '()
              coll))

(map square '(1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(append '(1 2 3) '(4 5 6))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y))
              0
              seq))

(length '(1 2 3 4 5))