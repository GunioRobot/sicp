#lang racket

#|

There will be an extra frame because of the `let' statements. 

The lambdas are evaluated only when called. So, if we put all the definitions on the top
before the statements which actually use them, then we already have 'sumultaneous scope'
implemented. Infact that is the assumption we had while implementing 4.16. The definitions
can contain references to other variables not defined yet, because they will be looked up
only when we call it with a value.

|#