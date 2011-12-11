#lang racket

#|

I think Eva's scheme is the correct one but very hard to implement. One way to do it for non-function 
definitions is to sort definitions in the order of least dependence on each other. If these definitions
(again, non-function) are mutually dependent, an error should be flagged.

|#