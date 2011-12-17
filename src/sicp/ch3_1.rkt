#lang racket

;; 3.1.1
(define balance 100)

(define (withdraw amount)
  (if (>= (- balance amount) 0)
      (begin
        (set! balance (- balance amount))
        balance)
      (print "insufficient balance")))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= (- balance amount) 0)
          (begin
            (set! balance (- balance amount))
            balance)
          "insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= (- balance amount) 0)
        (begin
          (set! balance (- balance amount))
          balance)
        "insufficent funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 200))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= (- balance amount) 0)
        (begin
          (set! balance (- balance amount))
          balance)
        "insufficient funds"))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) (lambda (amount) (withdraw amount)))
      ((eq? m 'deposit) (lambda (amount) (deposit amount)))
      (else (error "unknown request -- make-account " m))))
  dispatch)

;; 3.1.2

(define rand
  (let ((x rand-init))
    (lambda ()
      (begin
        (set! x (rand-update x))
        x))))

;; monte carlo simulation

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; using rand-update
(define (estimate-pi2 trials)
  (sqrt (/ 6 (random-gcd-trials trials random-init))))

(define (random-gcd-trials trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond
          ((= trials-ramaining 0) (/ trials-passed trials))
          ((= (gcd x1 x2) 1) (iter (- trials-remaining 1)
                                   (+ trials-passed 1)
                                   x2))
          (else (iter (- trials-remaining 1)
                      trials-passed
                      x2))))))
  (iter trials 0 initial-x))