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
  (define (dispatch given-password m)
    (if (eq? given-password password)
        (cond ((eq? m 'withdraw) (lambda (amt) (withdraw amt)))
              ((eq? m 'deposit) (lambda (amt) (deposit amt)))
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (temp) "Incorrect password")))
  dispatch)