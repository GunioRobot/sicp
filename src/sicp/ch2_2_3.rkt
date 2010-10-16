#lang racket/load
(require "utils.rkt")

(define (sum-odd-squares tree)
  (cond 
    ((null? tree) 0)
    ((not (pair? tree)) 
     (if (odd? tree) (square tree) 0))
    (else (+ (sum-odd-squares (car tree))
             (sum-odd-squares (cdr tree))))))
                           
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter pred? coll)
  (cond 
    ((empty? coll) '())
    ((pred? (car coll)) (cons (car coll) (filter pred? (cdr coll))))
    (else (filter pred? (cdr coll)))))

(define (accumulate op initial coll)
  (if (null? coll)
      initial
      (op (car coll)
          (accumulate op initial (cdr coll)))))

(define (enumerate-tree tree)
  (cond 
    ((null? tree) '())
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons 
              '()
              (filter even?
                      (map fib 
                           (range 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              '()
              (map square 
                   (map fib 
                        (range 0 n)))))

(define (product-of-squares-of-odd-elements coll)
  (accumulate *
              1
              (map square
                   (filter odd? coll))))

