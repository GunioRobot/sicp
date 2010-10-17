#lang racket

(require "utils.rkt"
         "ex2_36.rkt")

;; dot product
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

;; (matrix-*-vector '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '(1 2 3 4))

(define (transpose m)
  (accumulate-n cons '() m))

;; (transpose '((1 2 3) (4 5 6) (7 8 9)))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (v) (matrix-*-vector cols v)) m))) 

;; (matrix-*-matrix '((0 -1 2) (4 11 2)) '((3 -1) (1 2) (6 1)))