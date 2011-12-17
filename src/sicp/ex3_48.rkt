#lang racket

#|

by giving a unique number to each account, we are ordering the access to the account
in a specific manner (say in the ascending order of the number). The reason why deadlock
occurs is be cause of the concurrent access to the accounts in a specific order (Paul
attempts to transfer a1 and a2 while Peter attempts to transfer a2 and a1. With the
suggested change, both will concurrently try to access a1 first but only one of them
will get access while the other will wait until the serializer for a1 is released.

|#

(define (make-account-and-serializer account-number balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'account-number) account-number)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (number1 (account1 'account-number))
        (number2 (account2 'account-number)))
    (if (< number1 number2)
        ((serializer2 (serializer1 exchange))
         account1
         account2)
        ((serializer1 (serializer2 exchange))
         account1
         account2)
))