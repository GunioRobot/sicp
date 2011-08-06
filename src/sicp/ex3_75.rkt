#lang racket

#|

The procedure find average of the current sensor value with the last average value. This
is wrong. the symbol 'last-value' is used differently in different places. In the average
calculation, this should be the last sensor value. In the sign-change-detector this should
be last average value.

Solution is for the make-zero-crossing to accept the last-average-value as another parameter.

|#