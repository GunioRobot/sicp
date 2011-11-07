#lang racket

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((type exp) ((get (type exp)) exp env))))

(define (type expr) (car expr))

(put 'quote 
     (lambda (expr env)
       (text-of-quotation expr)))

(put 'set!
     (lambda (expr env)
       (eval-assignment expr env)))



     
