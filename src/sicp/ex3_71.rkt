#lang racket

(define (stream-duplicates s)
  (if (= (stream-car s) (stream-car (stream-cdr s)))
      (cons-stream (stream-car s)
                   (stream-duplicates (stream-cdr s)))
      (stream-duplicates (stream-cdr s))))

(define (cube-sum i j)
  (+ (* i i i) (* j j j)))

(display-stream (stream-duplicates (stream-map (lambda (p)
                                                 (cube-sum (car p) (car (cdr p))))
                                               (weighted-pairs integers integers cube-sum))))