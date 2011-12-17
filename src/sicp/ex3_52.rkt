#lang planet neil/sicp

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s)
                  (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred? s)
  (cond [(stream-null? s) the-empty-stream]
        [(pred? (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred? (stream-cdr s)))]
        [else (stream-filter pred? (stream-cdr s))]))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; stream-enumerate-interval
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1)
                                              high))))

#|

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)

(display-stream z)

|#

#|

What is the value of sum after each of the above expressions is evaluated?

ANS:

> (define seq (stream-map accum (stream-enumerate-interval 1 20)))
1

This is because (stream-enumerate-interval 1 20) builds up a stream
which looks as follows:

s ==

(cons 1
      (delay (cons 2
                   (delay (cons 3
                                (delay .......)))..))

Now, calling stream-map will produce seq which looks like this:

seq == (cons (accum (car s))  ;; adds (car s) to sum
             (delay (stream-map accum (stream-cdr s))))
    == (cons 1
             (delay (stream-map accum (stream-cdr s))))
|#

#|

(define y (stream-filter even? seq))

This will first run (even? (stream-car seq)) which will be false for
the first value (i.e. 1). So, it will do:

(stream-filter even? (stream-cdr seq))

Calling stream-cdr on the seq will 'force' the delayed computation.
So, seq becomes:

seq == (cons 1
             (stream-map accum (stream-cdr s)))
    == (cons 1
             (cons (accum 2) ;; sum becomes 3
                   (delay (stream-map accum (stream-cdr s)))))

Now, stream-filter will be called with seq as:

 (cons 3
       (delay (stream-map accum (stream-cdr s))))

Again, stream-filter will run the car of seq with the predicate:

 (even? (car seq))

which will be false; Now the new computation will be:

 (cons (accum 3) ;; sum becomes 3 + 3 = 6
       (delay (stream-map accum (stream-cdr s)))))

When stream-filter runs the predicate, (even? (stream-car seq))
it will return true. At this point, sum = 6.

|#

#|

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

At this point, seq is

(1 (3 (6 (delay (stream-map accum (stream-cdr s)))))

The forming of the new value of z will run it thru 1, 3 and 6
and finds that they are not multiple of 5. Now, seq is

(cons (accum 4) ;; sum = 6 + 4 = 10
      (delay (stream-map accum (stream-cdr s))))

=
(cons 10
      (delay (stream-map accum (stream-cdr s)))))

Now, stream-filter will run the predicate and will return
true and returns a stream-cons. At this point, sum = 10.

|#

#|

(stream-ref y 7)

This will take out 7 values out of y, which means 7 even values. So,
we produce 16 values in the initial stream before we get 7 even values.

(cons 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 (delay (cons 17 (delay (cons 18 ...))))

sum should be
(+ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
= 136

|#

#|

(display-stream z)

If we were to evaluate seq as a whole and also the rest, we would
get the following.

seq is:

(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)

y is

(10 28 36 66 78 120 136 190 210)

z is

(10 15 45 55 95 105 120 190 210)

sum is 210

|#