#lang racket

;; Consider the problem of transferring an amount from one account to
;; another. Ben Bitdiddle claims that this can be accomplished with the
;; following procedure, even if there are multiple people concurrently 
;; transferring money among multiple accounts, using any account 
;; mechanism that serializes deposit and withdrawal transactions, for 
;; example, the version of make-account in the text above.

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

#|

Since withdraw and deposit themselves are 'atomic' (or rather 'safe')
there is no problem with this routine. 

|#