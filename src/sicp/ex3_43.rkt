#lang racket

#|

;; Argue that if the processes are run sequentially, after any number of 
;; concurrent exchanges, the account balances should be $10, $20, and $30 
;; in some order. 

Support a1, a2 and a3 are the 3 accounts with the balances $10, $20 and $30
respectively. Now the operations involved in the serialized exchange are:
(in order)

(serialized-exchange a1 a2)

1. acquire serializer s1 for a1
2. acquire serializer s2 for a2
3. create serialized exchange procedure using s2
4. create serialized exchange procedure using s1
5. apply the resulting procedure of step 4 to a1 and a2.

Now, if we have two concurrent serialized exchange procedure running, one
of them is (serialized-exchange a1 a2) and the other is 
(serialized-exchange a1 a3), then the following is one possibility.

1. procedure 1 (called p1 from here after) acquires s1. 
2. p2 also tries to acquire s1 but since p1 has already acquired it, it waits.
3. p1 proceeds with the exchange. resulting in a swapped exchange of balance
   with a1 having 20 and a2 having 10.
4. At this point, s1 is released and p2 acquires s1. It proceeds to do the
   exchange, resulting in exchange of 20 and 30.
5. End result is: a1 = 30, a2 = 10, a3 = 20.

;; Draw a timing diagram like the one in figure 3.29 to show how this 
;; condition can be violated if the exchanges are implemented using the 
;; first version of the account-exchange program in this section.

Without serializer, the exchange procedure involves the following steps.

1. get balance of account1
2. get balance of account2
3. find difference between 1 and 2.
4. withdraw the difference from account1
5. deposit the difference to account2

When two concurrent exchange procedures (exchange a1 a2) and (exchange a1 a3)
are running, the following can happen.

1. p1 gets balance of a1 (10).
2. p2 gets balance of a1 (10).
3. p1 gets balance of a2 (20).
4. p2 gets balance of a3 (30).
5. p1 calculates difference (-10).
6. p2 calculates difference (-20).
7. p1 withdraws -10 from a1 (20).
8. p2 withdraws -20 from a1 (40).
9. p1 deposits -10 to a2 (10)
10. p2 deposits -20 to a3 (10)

At the end, a1 = 40, a2 = 10, a3 = 10

;; On the other hand, argue that even with this exchange program, the sum of 
;; the balances in the accounts will be preserved. 

(see above. We can try with different interspersing of operations. Note that
the above steps assume that withdrawals and deposits and balance operations
are 'atomic')

;; Draw a timing diagram to show how even this condition would be violated 
;; if we did not serialize the transactions on individual accounts. 

It is obvious that this will be violated. For example, withdraw operation
involves (set! balance (- balance amount)) and deposit operation involves
(set! balance (+ balance amount)). It could be that, after the target
balance is calculated for an account, a new balance amount is set which
is reset by the previously calculated value of balance. This is a non-linear
operation and not a composition. 

|#