#lang racket

(define (integral integrand delayed-initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-initial-value)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))