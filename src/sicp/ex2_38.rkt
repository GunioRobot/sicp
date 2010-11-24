#lang racket

(define (fold-left op initial coll)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial coll))

(provide fold-left)