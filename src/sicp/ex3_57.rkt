#lang racket

#|

We do (n - 1) additions for the memo-proc based delay implementation.

With the simple delay implementation, for a given n, it constructs the
fib sequence:

 fib (n) = fib (n-1) + fib (n-2)

In our case, the number of additions for nth fib will be equal to the
number of additions for the fib(n-1) and that for fib(n-2) + 1

|#