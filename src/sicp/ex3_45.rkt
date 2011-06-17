#lang racket

#|

Deposits and Withdrawals are fine. But when it comes to exchange, we need a
"mutually serialized" procedure with something like this:

 (serializer-1 (serializer-2 exchange))

Otherwise, we are creating a "time hole" when another execution process can
get hold of one of the account and deposit/withdraw from it which will make
the end result of exchange erroneous.

|#