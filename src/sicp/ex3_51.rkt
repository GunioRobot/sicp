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

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

This will display 0. The reason is that, we can reduce this expression
to

(cons
     (show (car
                (cons 0 (delay (stream-enumerate-interval 1 10))))
     (delay
           (stream-map show (stream-cdr
                                       (cons 0 (delay (stream-enumerate-interval 1 10)))))))

This will print 0 and the rest of the computation is delayed.

(stream-ref x 5)

stream-ref will keep calling stream-cdr on the stream for 5 times. This will 'force' evaluation
of the delayed operations.

x is (cons 0
           (delay (stream-map show (stream-enumerate-interval 1 10))))

Calling stream-cdr once, this expression becomes
 (cons 0
       (cons-stream (show (stream-car (stream-enumerate-interval 1 10)))
                    (stream-map show
                                (stream-cdr (stream-enumerate-interval 1 10)))))

=

 (cons 0
       (cons 1 ;; also prints 1
             (delay (stream-map show
                                (stream-cdr (stream-enumerata-interval 1 10))))))

As can be seen, this will print 1 and the rest are 'delayed'.

Think of the output of (stream-enumerate-interval 0 10) as:

(cons 0
      (delay (cons 1
                   (delay (cons 2
                                (delay (cons 3
                                             (delay (cons 4
                                                          .........
                                                                   (delay (cons 10 the-empty-stream))...)

Let us call it s.

Now, x is (stream-map show s). This is equiv of:

(cons (show (stream-car s))
      (delay (stream-map show (stream-cdr s)))

This will first print 0, when defined.

(stream-ref 5 x) is equiv to:

(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr x))))))

This will make the stream-map work on the 5 delayed computations and hence
numbers from 1 to 5 printed on the screen. Together will that, the value of
(stream-ref s 5) == 5 will also be printed.

> (stream-ref x 7)

This will print 6 and 7 along with the value of the expression (i.e. 7).

|#

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

