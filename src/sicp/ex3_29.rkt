#lang racket

#|
(or a b) = (and (and (not a) b)
                (and a (not b))
|#

(define (or-gate o1 o2 output)
  (let ((a1 (make-wire)) 
        (a2 (make-wire))
        (a3 (make-wire))
        (a4 (make-wire)))
    (inverter o1 a1)
    (inverter o2 a2)
    (and-gate a1 o2 a3)
    (and-gate o1 a2 a4)
    (and-gate a3 a4 output)
    'ok))

#|

or-gate delay is one inverter delay + 2 x and-gate-delay, considering that
the whole things works in a parallel way. i.e. a3 and a4 are produced parallely.

|#