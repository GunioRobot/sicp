#lang racket

;; given a frame, get the value of a given variable
;; false if not var is not found in the frame
(define (get-var-in-frame var frame)
  ...)

;; set a value for a given variable in a frame
;; if frame already contain var, change its binding, else
;; if frame does not have a binding for var, then create one.
(define (set-var-in-frame! var val frame)
  ...)

;; now using these two procedures, expressing `lookup-variable-value'
(define (lookup-variable-value var env)
  (define (env-loop env)
    (let ((frame (first-frame env)))
      (let ((var-val (get-var-in-frame var frame)))
        (if var-val
            var-val
            (env-loop (enclosing-environment env))))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (env-loop env)))


(define (set-variable-value! var val env)
  (define (env-loop env)
    (let ((frame (first-frame env)))
      (let ((var-val (get-var-in-frame var frame)))
        (if var-val
            (set-var-in-frame! var val frame)
            (env-loop (enclosing-environment env))))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (env-loop env)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (set-var-in-frame! var val frame)))
