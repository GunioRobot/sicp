#lang racket

;; part a
(define (integrate-series stream)
  (define (integrate-iter S I)
    (stream-map (lambda (s i)
                  (* s (/ 1 i)))
                S
                I))
  (integrate-iter stream
                  (integers-starting-from 1)))

;; part b
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
    (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))



