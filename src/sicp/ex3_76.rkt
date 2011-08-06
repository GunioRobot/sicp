#lang racket

(define (smooth stream)
  (cons-stream (/ (+ (stream-car stream)
                     (stream-car (stream-cdr stream))) 
                  2.0)
               (smooth (stream-cdr stream))))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings (smooth sense-data) 0))
               
               