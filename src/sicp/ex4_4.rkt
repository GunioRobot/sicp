#lang racket

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; and
(define (and? expr) (tagged-list? expr 'and))
(define (eval-and expr env) (eval-and-predicates (cdr expr) env))

(define (eval-and-predicates expr env)
  (cond ((null? expr) #t)
        ((eval (car expr) env) (eval-and-predicates (cdr expr) env))
        (else #f)))

;; or
(define (or? expr) (tagged-list? expr 'or))
(define (eval-or expr env) (eval-or-predicates (cdr expr) env))

(define (eval-or-predicates expr env)
  (cond ((null? expr) #f)
        ((eval (car expr) env) #t)
        (else (eval-or-predicates (cdr expr) env))))

;; derived expressions
(define (and->if expr)
  (expand-and-predicates (cdr expr)))

(define (expand-and-predicates expr)
  (cond ((null? expr) '#t)
        (else (make-if (car expr)
                       (expand-and-predicates (cdr expr))
                       '#f))))

(define (or->if expr)
  (expand-or-predicates expr))

(define (expand-or-predicates expr)
  (cond ((null? expr) '#f)
        (else (make-if (car expr)
                       '#t
                       (expand-or-predicates (cdr expr))))))