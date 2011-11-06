#lang racket

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! input invert-input)
  'ok)

(define (logical-not input)
  (cond ((= input 0) 1)
        ((= input 1) 0)
        (else (error "LOGICAL-NOT: invalid input value" input))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; wires
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'get-signal)  signal-value)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- wire" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire value)
  ((wire 'set-signal!) value))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))

