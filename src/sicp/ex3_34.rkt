#lang racket

#|

If Louis Reasoner implements the squarer with a multiplier, like this:

(define (squarer a b)
  (multiplier a a b))

let us see how we will use it.

(define a (make-connector))
(define b (make-connector))

(set-value! a 10 'user)

At this point, we expect b to have the value 100. Indeed it is. Now, instead
we set the value of b to be 400. But multiplier needs two values to operate.
We have set only b. So, the contraint never propagates to a. Here is the
output of a repl session:

> (set-value! e 400 'user)

Probe: squarer output = 400
'done
>

|#