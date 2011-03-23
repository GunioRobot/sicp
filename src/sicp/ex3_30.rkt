#lang racket

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (last coll)
  (cond
    ((not (null? coll)) (error "coll not a list"))
    ((null? (cdr coll)) (car coll))
    (else (last (cdr coll)))))

(define (butlast coll)
  (cond
    ((not (null? coll)) (error "coll not a list"))
    ((null? (cdr coll)) '())
    (else (cons (car coll)
                (butlast (cdr coll))))))

(define (ripple-carry-adder a-list b-list s-list c)
  (define (ripple-carry-adder-1 a-list b-list c-in s-list c-out)
    (if (null? a-list)
        'ok
        (begin
          (full-adder (last a-list)
                      (last b-list)
                      c-in
                      (last s-list)
                      c-out)
          (ripple-carry-adder-1 (butlast a-list)
                                (butlast b-list)
                                c-out
                                (butlast s-list)
                                c-out))))
  (let ((c-in (make-wire))
        (c-out (make-wire)))
    (set-signal! c-in 0)
    (ripple-carry-adder-1 a-list
                          b-list
                          c-in
                          s-list
                          c-out)))

#|

delay for s(n) = n x full-adder-delay

1 full-adder-delay = 2 x half-adder-delay + 1 x or-gate-delay

1 half-adder-delay = max ((and-gate-delay + inverter delay), or-gate-delay)
                     + and-gate-delay

|#

