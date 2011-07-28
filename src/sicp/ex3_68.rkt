#lang racket

#|

'interleave' is a procedure and not a special form because of which the arguments
of the interleave will be evaluated before interleave itself is called. This will
result in an infinite number of calls to 'pair'.

|#