#lang racket

#|

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

list all possible values of x.

|#

#|

1. 100
2. 1000
3. (expt 100 3) = 1000000
4. (expt (expt 10 3) 2) = 1000000
5. (* 10 (* 10 10 10)) = 10000
6. (* 10 (* 10 10) (* 10 10)) = 100000
7. (* 10 10 (* 10 10)) = 10000

|#

#|

Which of these possibilities remain if we instead use serialized procedures:

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

ans:

=> 1000000

|#