#lang racket
;; sets as ordered lists.

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= (car set) x) #t)
        ((> (car set) x) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) 
             (null? set2)) '())
        ((= (car set1) 
            (car set2)) 
         (cons (car set1) (intersection-set (cdr set1) 
                                            (cdr set2))))
        ((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
        ((> (car set1) (car set2)) (intersection-set set1 (cdr set2)))))