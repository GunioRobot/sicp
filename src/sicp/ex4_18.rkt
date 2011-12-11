#lang racket

#|

It won't work in the new definition because the new let translates to a lambda and application
of the lambda to the expressions. When we do that, evaluation of one expression needs the other.

It will work with the definition given in the text because the expressions are inside the lambda.

|#