#lang racket

(require "ex2_69.rkt")

(define (range min max (step 1))
  (if (>= min max)
      '()
      (cons min (range (+ min step) max step))))

;; n = 5
(define tree5 (for/list ((x (range 0 5))
                         (a '(A B C D E)))
                (list a (expt 2 x))))

(generate-huffman-tree tree5)
#|
Most frequently used symbol is E and has the code 1
Least frequently used symbol is A and has the code 0000
|#

;; n = 10
(define tree10 (for/list ((x (range 0 10))
                          (a '(A B C D E F G H I J)))
                 (list a (expt 2 x))))

(generate-huffman-tree tree10)
#|
A - 000000000
J - 1
|#