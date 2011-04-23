#lang racket

#|

If we don't call the proc in the add-action! procedure then the outputs for the 
given inputs will be in some undefined default states. They won't reflect the 
logic that the function blocks are representing.

Let us take half adder example:

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

The gate procedures like inverter and or-gate calls add-action! which
is the procedure which adds the gate operation to the agenda. If it
is not called once, then the operation is not added into the agenda.

If accept-action-proc! is defined without the call to proc, i.e.

(define (accept-action-proc! proc)
  (set! action-procedures (cons proc action-procedures))

then, let us see what happens.

The or-gate definition will call add-action on both its inputs a and b. make-wire 
by default sets the wire value as 0. So, for the default case, the inputs to the
half-adder will be 0 and 0 and the output of the or-gate and and-gate will be 0.
i.e. D = 0, C = 0.
E will also be 0. So, S = 0 C = 0, irrespective of the initial values of a and b.

|# 