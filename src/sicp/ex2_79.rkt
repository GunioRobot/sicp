#lang racket

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (define (equ? x y)
    (= x y))
  (put 'equ? '(scheme-number scheme-number) equ?)
  'done)

(define (install-rational-number-package)
  (define (equ? r1 r2)
    (let* ([n1 (numer r1)]
           [d1 (denom r1)]
           [n2 (numer r2)]
           [d2 (denom r2)]
           [g1 (gcd n1 d1)]
           [g2 (gcd n2 d2)])
      (and (= (/ n1 g1) (/ n2 g2))
           (= (/ d1 g1) (/ d2 g2)))))
  (put 'equ? '(rational rational) equ?)
  'done)

(define (install-complex-number-package)
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex complex) equ?)
  'done)
