#lang racket

(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  (define (=zero? x)
    (zero? x))
  (put '=zero? '(scheme-number) =zero?)
  'done)

(define (install-rational-number-package)
  (define (=zero? r)
    (zero? (numer r)))
  (put '=zero? '(rational) =zero?)
  'done)

(define (install-complex-number-package)
  (define (=zero? z)
    (and (zero? (real-part z))
         (zero? (imag-part z))))
  (put '=zero? '(complex) =zero?)
  'done)

