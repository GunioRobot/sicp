#lang racket

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define (sign-change-detector current-value previous-value)
  (cond [(and (positive? previous-value)
              (positive? current-value)) 0]
        [(and (positive? previous-value)
              (negative? current-value)) -1]
        [(and (negative? previous-value?)
              (positive? current-value)) +1]
        [else 0]))

(define zero-crossings (make-zero-crossings sense-data 0))

;; eva's implementation
(define zc (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))
