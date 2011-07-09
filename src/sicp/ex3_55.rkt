#lang racket

(define (partial-sums s)
  (define (partial-sums-iter sum stream)
    (cons-stream (+ sum (stream-car stream))
                 (partial-sums-iter (+ sum (stream-car stream))
                                    (stream-cdr stream))))
  (partial-sums-iter 0 s))

#|
> (stream-ref (partial-sums (integers-starting-from 1)) 0)
1
> (stream-ref (partial-sums (integers-starting-from 1)) 1)
3
> (stream-ref (partial-sums (integers-starting-from 1)) 2)
6
> (stream-ref (partial-sums (integers-starting-from 1)) 3)
10
> (stream-ref (partial-sums (integers-starting-from 1)) 4)
15
|#