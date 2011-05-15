#lang racket

#|

I don't agree. 'balance' is only read and for a particular account, it is 
not usually read while another transaction is in progress. Further, even if
it is read concurrently with a transaction (deposit or withdrawal), one will
get either the value before the transaction or the one after. Both are 
acceptable.

|#