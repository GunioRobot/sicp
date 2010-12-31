#lang racket

(define (adjoin-term term term-list) 
  (cons (coeff term) term-list))

(define (the-empty-termlist) '())

(define (first-term term-list)
  (let ((len (length term-list)))
    (make-term (- len 1) (car term-list))))

(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff)
(define (order term) (car term))
(define (coeff term) (cadr term))