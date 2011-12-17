#lang racket

#|

The output is computed at that instant when the gate
function is called. This value of the output is used
by set-signal! during the required time segment. So,
we don't compute the gate output at the time slot. It
is already precomputed at definition time. So, we should
also output them in the same order.

|#
