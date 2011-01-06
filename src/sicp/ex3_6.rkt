#lang racket

(define rand
  (let ((x random-init))
    (define (dispatch type)
      (cond
        ((eq? type 'generate)
         (begin
           (set! x (rand-update x))
           x))
        ((eq? type 'reset)
         (lambda (new-val)
           (set! x new-val)))))
    dispatch))


