#lang racket

(require "ch2_3_3.rkt")

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

 (union-set '(1 2 3 4) '(5 6 7 8))
 (union-set '(1 2 3 4) '(1 2 3 4))
 (union-set '(1 2 3 4) '(1 2 7 8))