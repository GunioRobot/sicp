#lang racket

#|

Writing down the pairs, one can sort of form the following judgement.

For any particular pair of the form S0Tm, the number of terms which precedes it
is (+ (* 2 (- m 1)) 1) for m >= 2

For a pair of the form S1Tm, it is (* (- m 1) 4) for m >= 2.

Similarly one can get working formulae for other row, column pairs.

|#