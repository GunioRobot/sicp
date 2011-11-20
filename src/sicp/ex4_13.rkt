#lang racket

(define (make-unbound! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (error "variable not bound" var))
            ((eq? var (car vars))
             (begin
               (set-car! vals (cdr vals))
               (set-cdr! vars (cdr vars))))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

#|

Remove binding from the first frame, as removing it from elsewhere
would mean also removing all bindings built from it.

|#