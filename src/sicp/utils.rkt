#lang racket

(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; naive fibonacci definition
(define (fib n)
  (cond 
    ((or (= n 0) (= n 1)) 1)
    (else (+ (fib (- n 1))
             (fib (- n 2))))))

(define (range low high (step 1))
  (cond 
    ((or (and (< low high)
              (positive? step))
         (and (> low high)
          (negative? step)))
     (cons low (range (+ low step) high step)))
    (else '())))

(define (accumulate op initial coll)
  (if (empty? coll)
      initial
      (op (car coll)
          (accumulate op initial (cdr coll)))))

(provide square fib range accumulate)    