#lang racket/base

(provide let->combination)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (let? expr)
  (tagged-list? expr 'let))

(define (let-bindings expr) (cadr expr))
(define (let-bindings-variables bindings) (map car bindings))
(define (let-bindings-values bindings) (map cadr bindings))
(define (let-body expr) (cddr expr))

(define (let->combination expr)
  (let ([bindings (let-bindings expr)])
    (let ([vars (let-bindings-variables bindings)]
          [vals (let-bindings-values bindings)]
          [body (let-body expr)])
      (cons (make-lambda vars body) vals))))

(define ns (make-base-namespace))
(eval (let->combination '(let ((x 2) (y 3)) (+ x y))) ns)
