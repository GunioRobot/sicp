#lang racket

;; for scheme-numbers
(put 'cos '(scheme-number)
     (lambda (x) (cos x)))

(put 'cos '(real)
     (lambda (x) (cos x)))

(put 'cos '(rational)
     (lambda (r) (exact->inexact r)))

;; similarly define sin, atan, square
