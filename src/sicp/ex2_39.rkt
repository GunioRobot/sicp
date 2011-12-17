#lang racket

(require "utils.rkt"
         "ex2_36.rkt"
         "ex2_38.rkt")

(define fold-right accumulate)

(define (reverse1 coll)
  (fold-right (lambda (x y) (append y (list x))) '() coll))

(define (reverse2 coll)
  (fold-left (lambda (x y) (cons y x)) '() coll))

