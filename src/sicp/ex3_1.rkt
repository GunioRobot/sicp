#lang racket

(define (make-accumulator acc)
  (lambda (val)
    (begin
      (set! acc (+ acc val))
      acc)))