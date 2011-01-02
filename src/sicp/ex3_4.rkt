#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((bad-password-count 0))
    (define (dispatch given-password m)
      (if (eq? given-password password)
          (cond ((eq? m 'withdraw) (lambda (amt) (withdraw amt)))
                ((eq? m 'deposit) (lambda (amt) (deposit amt)))
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))
          (lambda (temp) 
            (begin
              (set! bad-password-count (+ bad-password-count 1))
              (if (>= bad-password-count 7)
                  "Something is wrong. Calling 911..."
                  "Incorrect password")))))
    dispatch))

#|

> (define acc (make-account 100 'secret-password))
> ((acc 'secret-password 'withdraw) 40)
60
> ((acc 'some-other-password 'deposit) 50)
"Incorrect password"
> ((acc 'some-other-password 'deposit) 50)
"Incorrect password"
> ((acc 'some-other-password 'deposit) 50)
"Incorrect password"
> ((acc 'some-other-password 'deposit) 50)
"Incorrect password"
> ((acc 'some-other-password 'deposit) 50)
"Incorrect password"
> ((acc 'some-other-password 'deposit) 50)
"Incorrect password"
> ((acc 'some-other-password 'deposit) 50)
"Something is wrong. Calling 911..."
> 

|#