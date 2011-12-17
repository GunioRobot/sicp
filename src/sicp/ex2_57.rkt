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
        ((exponentiation? exp) (let ((u (base exp))
                                     (n (exponent exp)))
                                 (make-product n
                                               (make-product (make-exponentiation u (make-sum n -1))
                                                             (deriv u var)))))
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
        (else (list '+ x y))))

(define (make-product x y)
  (cond ((equal? x 1) y)
        ((equal? y 1) x)
        ((equal? x 0) 0)
        ((equal? y 0) 0)
        (else (list '* x y))))

(define (sum? exp)
  (and (pair? exp)
       (eq? (car exp) '+)))

(define (product? exp)
  (and (pair? exp)
       (eq? (car exp) '*)))

(define (addend exp) (car (cdr exp)))

(define (augend exp)
  (define (augend* e1 . en)
    (cond ((null? en) e1)
          (else (make-sum e1 (apply augend* (car en) (cdr en))))))
  (apply augend* (cdr (cdr exp))))

(define (multiplicant exp) (car (cdr exp)))

(define (multiplier exp)
  (define (multiplier* e1 . en)
    (cond ((null? en) e1)
          (else (make-product e1 (apply multiplier* (car en) (cdr en))))))
  (apply multiplier* (cdr (cdr exp))))

;; exponentiation
(define (exponentiation? exp) (eq? (car exp) '**))
(define (base exp) (car (cdr exp)))
(define (exponent exp) (car (cdr (cdr exp))))
(define (make-exponentiation base exp)
  (cond ((and (number? exp) (zero? exp)) 1)
        ((and (number? exp) (= exp 1)) base)
        (else (list '** base exp))))
