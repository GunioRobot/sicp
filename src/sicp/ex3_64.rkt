#lang planet neil/sicp

;; stream-of-numbers -> floating point number -> number
(define (stream-limit s limit)
  (let ([num0 (stream-car s)]
        [num1 (stream-car (stream-cdr s))])
    (let ([diff (- num0 num1)])
      (if (< (abs diff) limit)
          num1
          (stream-limit (stream-cdr s) limit)))))