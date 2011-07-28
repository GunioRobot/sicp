#lang racket

(define (triples s t u)
  (cons-stream (append (stream-car (pairs s t))
                       (list (stream-car u)))
               (interleave
                (stream-map (lambda(p) (append p (list (stream-car u))))
                            (stream-cdr (pairs s t)))
                (triples (stream-cdr s)
                         (stream-cdr t)
                         (stream-cdr u)))))