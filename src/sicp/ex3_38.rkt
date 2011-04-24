#lang racket

#|

Peter: 	(set! balance (+ balance 10))
Paul: 	(set! balance (- balance 20))
Mary: 	(set! balance (- balance (/ balance 2)))

1.

a. Peter deposits $10
b. Paul withdraws $20
c. Mary withdraws half of balance.

100 + 10 - 20 = 90 -> 90/2 -> $45

2. 

a. Peter deposits $10
b. Mary withdraws half of balance.
c. Paul withdraws $20

100 + 10 -> 55 - 20 -> $35

3.

a. Paul withdraws $20
b. Peter deposits $10
c. Mary withdraws half of balance.

100 - 20 + 10 -> 110 /2 -> $55

4. 

a. Paul withdraws $20
b. Mary withdraws half of balance.
c. Peter deposits $10

100 - 20 => 80 / 2 => 40 + 10 => 50

5.

a. Mary withdraws half of balance.
b. Peter deposits $10
c. Paul withdraws $20

100 / 2 => 50 + 10 - 20 => 40

6.

a. Mary withdraws half of balance.
b. Paul withdraws $20
c. Peter deposits $10

100 / 2 => 50 - 20 + 10 => 40

|#

#|

Balance could take many values if the three transactions are allowed to
happen concurrently.

It could be 110, 80 or 50 if one of them mutates the balance variable
at the end depending on which one manages to finish last. It could also
be that 2 of them work concurrently and the resultant value is processed
by the third transaction.

|#