#lang racket

(define (make-monitored f)
  (let ((num-calls 0))
    (lambda (x)
      (cond 
        ((eq? x 'how-many-calls?) num-calls)
        (else 
         (begin
           (set! num-calls (+ num-calls 1))
           (f x)))))))

#|
> (define (square x) (* x x))
> (square 4)
16
> (define mf (make-monitored square))
> (mf 'how-many-calls?)
0
> (mf 10)
100
> (mf 'how-many-calls?)
1
> (mf 100)
10000
> (mf 'how-many-calls?)
2
> 
|#