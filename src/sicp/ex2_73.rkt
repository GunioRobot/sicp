#lang racket

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [else ((get 'deriv (operator exp)) (operands exp)
                                       var)]))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; part a
#|

The 'get' procedure fetches from the table, an appropriate lambda
function which takes as input, the operands and the variable. The
function will return the appropriate expressions for addition and
multiplication.

In theory we can define dispatch functions for number? and variable?.
But for those functions, the dispatch functions will return a
constant regardless of the input operands.
|#

;; part b
(define deriv-sum
  (lambda (exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var))))

(define deriv-prod
  (lambda (exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp)))))

(define (install-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  ;; public procedures
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod))

;; part c
(define deriv-exponentiation
  (lambda (exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentiation (base exp)
                                                     (- (exponent exp) 1))
                                (deriv (base exp) var)))))

(define (make-exponent base exp)
  (list '** base exp))
(define (base x) (car x))
(define (exponent x) (cadr x))
(put 'deriv '** deriv-exponentiation)

;; part d.
#|
If we modify get, we just modify put and nothing else.
|#