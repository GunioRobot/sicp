#lang racket

;; 4.9
#|

do, for, while, until, when

a. for

(for <var> in <seq>
  <body>)

bind var with each value in sequence, execute the <body> and 
collect the result of the last expression into a list.

b. (dotimes n <number>
     <body>)

Bind n to the number and execute the body that many times for
side effects.

c. (while <predicate>
     <body>)

Check the value of the predicate, if true, execute the body,
check the predicate again until it is false, in which case,
stop executing the body.

|#

#|  implementation:

Derived expressions can be used to implement all the above 
constructs. For example, 'for' can be implementing by transforming
the body into an iterative procedure. 'dotimes' is similar and
simpler. 'while' can be implemented using recursive procedure
and 'if'.

|#

