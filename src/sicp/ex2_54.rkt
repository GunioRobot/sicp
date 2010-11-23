#lang racket

(define (equal? s1 s2)
  (cond ((null? s1) (null? s2))
        ((and (symbol? s1) 
              (symbol? s2)) (eq? s1 s2))
        ((and (number? s1) 
              (number? s2)) (= s1 s2))
        ((and (pair? s1)
              (pair? s2)
              (equal? (car s1) 
                      (car s2))) (equal? (cdr s1) (cdr s2)))
        (else #f)))
