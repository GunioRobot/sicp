#lang racket

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
#|

If there are two concurrent processes doing the above test-and-set! function,
there could be many things that can happen.

Assume that the cell is having a #f value initially. Process 1 tests the
value and finds that it is false. At the same instant, Process 2 also tests
the cell and finds that it is false and both of them set the cell to true at
the same instant. Both the processes get a false value from test-and-set!
and think that they are holding the mutex.

Another scenario is when the Process 1 does the test and then finds that the
cell is false. Next Process 2's test is executed and it also finds that the
cell is false. Now, both of them proceed to do a set and so both gets the
mutex.

In reality, only one of the processes should hold a mutex (Mutual exclusion).
So, that assumption is violated in this case. As footnote 47 indicate, if the
instruction that atomically implement test-and-set! is executed at the same
cycle by two separate concurrent processes, a hardware arbiter resolves who
gets the chance to execute it.

|#
