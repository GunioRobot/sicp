#lang racket

#|

memo-proc cannot handle recursive definitions. With the way it is defined in the question,
we have a recursive 'call' to sqrt-stream and hence produced another entirely different
stream whereas in the original definition, 'guesses' gets forced.

|#