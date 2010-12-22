#lang racket

(define (attach-tag tag contents)
  (cons tag contents))

(define (integer->rational x)
  (if (integer? x)
      (attach-tag 'rational (cons (contents x) 1))
      (error "input is not an integer")))

;; install in integer
(put 'raise '(rational)
     (lambda (x) (integer->rational x)))
         
(define (integer->real x) 
  (* x 1.0))

(define (rational->real r)
  (let ((n (numer r))
        (d (denom r)))
    (make-real (/ (integer->real n) d))))

;; install into rational package
(put 'raise '(real)
     (lambda (r) (rational->real r)))

(define (real->complex r)
  (make-complex-from-real-imag r 0))

;; install into real package
(put 'raise '(complex)
     (lambda (r) (real->complex r)))

(define (raise x)
  (apply-generic 'raise x))
