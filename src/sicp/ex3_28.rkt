#lang racket

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1)
                       (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (define (logicval-or o1 o2)
    (cond
      ((and (= o1 0) (= o1 0)) 0)
      ((or  (= o1 1) (= o2 1)) 1)
      (else (error "invalid signals"))))

  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

