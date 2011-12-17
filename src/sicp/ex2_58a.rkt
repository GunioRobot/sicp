#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicant exp) var))
                                  (make-product (multiplicant exp)
                                                (deriv (multiplier exp) var))))
        (else (error "unknown type of expression - deriv" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

;; sum
(define (make-sum x y)
  (cond ((equal? x 0) y)
        ((equal? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        ((equal? x y) (make-product 2 x))
        (else (list x '+ y))))

(define (make-product x y)
  (cond ((equal? x 1) y)
        ((equal? y 1) x)
        ((equal? x 0) 0)
        ((equal? y 0) 0)
        (else (list x '* y))))

(define (sum? exp)
  (and (pair? exp)
       (eq? (car (cdr exp)) '+)))

(define (product? exp)
  (and (pair? exp)
       (eq? (car (cdr exp)) '*)))

(define (addend exp) (car exp))
(define (augend exp) (car (cdr (cdr exp))))

(define (multiplicant exp) (car exp))
(define (multiplier exp) (car (cdr (cdr exp))))

;; test
(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'y)