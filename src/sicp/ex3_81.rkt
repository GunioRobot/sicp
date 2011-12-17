#lang racket

(define (rand-seq init req-stream)
  (let ((req (stream-car req-stream)))
    (cond ((eq? req 'reset)
           (let ((new-init (stream-car (stream-cdr req-stream))))
             (let ((new-rand-val (rand-update new-init)))
               (cons-stream
                new-rand-val
                (rand-seq new-rand-val
                          (stream-cdr (stream-cdr req-stream)))))))
          ((eq? req 'generate)
           (let ((new-val (rand-update init)))
             (cons-stream
              new-val
              (rand-seq new-val (stream-cdr req-stream))))))))
