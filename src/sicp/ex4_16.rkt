#lang racket

;; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (let ([value (car vals)])
               (if (eq? value '*unassigned*)
                   (error "evaluating a variable which has not been assigned -- LOOKUP-VARIABLE-VALUE:" value)
                   value))]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; b
(define (tagged-list? expr tag)
  (and (pair? expr) (eq? (car expr) tag)))

(define (definition? expr)
  (tagged-list? expr 'define))

(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-parameters expr)
  (car (cdr expr)))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

;;(lambda (x) (..)(..)...(..))
(define (lambda-body expr)
  (cdr (cdr expr)))

(define (scan-out-defines body)
  (define (internal-definition-names defs)
    (cond
      [(empty? defs) '()]
      [else (cons (cadr (car defs)) (internal-definition-names (cdr defs)))]))
  (define (internal-definition-values defs)
    (cond
      [(empty? defs) '()]
      [else (cons (caddr (car defs)) (internal-definition-values (cdr defs)))]))

  ;; assumes that definitions come first in the body
  (letrec ([get-body-internal (lambda (b)
                                (cond
                                  [(empty? b) '()]
                                  [(definition? (car b)) (get-body-internal (cdr b))]
                                  [else (cons (car b)
                                              (get-body-internal (cdr b)))]))]
           [get-definitions (lambda (b)
                              (cond
                                [(empty? b) '()]
                                [(definition? (car b)) (cons (car b) (get-definitions (cdr b)))]
                                [else '()]))])
    (let ([internal-definitions (get-definitions body)])
      (let ([def-names (internal-definition-names internal-definitions)]
            [def-vals (internal-definition-values internal-definitions)]
            [internal-body (get-body-internal body)])
        (letrec ([p (lambda (name-list)
                      (cond
                        [(empty? name-list) '()]
                        [else (cons (list (car name-list) ''*unassigned*)
                                    (p (cdr name-list)))]))]
                 [s (lambda (names vals)
                      (cond
                        [(empty? names) '()]
                        [else (cons (list 'set! (car names) (car vals))
                                    (s (cdr names) (cdr vals)))]))])
          (cons 'let
                (cons (p def-names)
                      (append (s def-names def-vals)
                              internal-body))))))))

;; c

#|
It is better to do this change in the procedure-body so that the procedure abstractions
are maintained as it is. The change we are introducing is related to Scheme and hence
should be done at the lowest level. We should be able to replace procedure-body for another
language and still be able to use the same eval function.

|#

(define (procedure-body p) (scan-out-defines (caddr p)))