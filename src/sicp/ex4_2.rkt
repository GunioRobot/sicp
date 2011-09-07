#lang racket

#|

a. application? only checks whether the given expression is a pair. It assumes that the rest of
   the built in language clauses are already handled. So this will upset all other classes like
   define, let where Louis's eval would think define is a procedure and would proceed to evaluate
   the operands, which is wrong.

|#

#|

b.

We only change the selectors and the predicate procedures, everything else remain the same to
implement the new proposal.

|#

(define (application? exp)
  (if (pair? exp)
      (tagged-list? expr 'call)
      #f))

(define (operator exp) (car (cdr exp)))
(define (operands exp) (cdr (cdr exp)))

