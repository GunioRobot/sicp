#lang r5rs

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1))
             (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond
               ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (memo-fib (- n 1))
                        (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((previous-output (make-table)))
    (lambda (x)
      (let ((lookup-result (lookup x previous-output)))
        (if lookup-result
            lookup-result
            (let ((result (f x)))
              (insert! x result previous-output)
              result))))))

#|

If we define memo-fib as

(define memo-fib
  (memoize fib))

and say, call (memo-fib 10), then, we cache the result of calling fib with n=10.
But (fib n) calls (fib (- n 1)) and (fib (- n 2)) and this way of calling memo-fib won't cache those intermediate values.

On the other hand, if we define memo-fib the way it is defined above, all intermediate calls to memo-fib also goes through memoize call and hence will get cached in the results table. Hence this is much more efficient.

|#
