#lang racket

(define (RLC R L C dt)
  (define (rlc1 vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define dvc0 (scale-stream il (/ -1 C)))
    (define il (integral (delay dil) il0 dt))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (/ -R L))))
    (stream-map (lambda (x y) (cons x y)) vc il)))


