#lang racket

(define f
  (let ((x 0))
    (lambda (y)
      (set! x (+ x 1))
      (cond
        ((and (= x 1) (= y 0)) -1)
        ((and (= x 2) (= y 1)) 1)
        ((and (= x 1) (= y 1)) 0)
        ((and (= x 2) (= y 0)) 1)))))
