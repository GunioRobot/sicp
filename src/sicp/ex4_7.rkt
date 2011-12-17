#lang racket

(provide let*? let*->nested-lets)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (let*? expr)
  (tagged-list? expr 'let*))

(define (let*-bindings expr) (cadr expr))
(define (let*-body expr) (cddr expr))


(define (let*->let bindings body)
  (cond [(empty? bindings) '()]
        [else
         (let ([binding (car bindings)]
               [rest-bindings (cdr bindings)])
           (if (empty? rest-bindings)
               (cons 'let (cons (list binding) body))
               (cons 'let (cons (list binding) (list (let*->let rest-bindings body))))))]))

#|

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let*->let bindings body)
  (cond [(empty? bindings) body]
        [else
         (make-let (list (car bindings))
                   (list (let*->let (cdr bindings) body))]))
|#

(define (let*->nested-lets exp)
  (let ([bindings (let*-bindings exp)]
        [body     (let*-body exp)])
    (let*->let bindings body)))

;; b
#|

It is enough to add an action for let* expression in eval, as eval
gets recursively called for the transformed expressions (assuming that
eval has case handlers for let expression whose action is to transform
into lambda expression application and eval it).

if we add the following action for let* expressions in eval:

(eval (let*->nested-lets exp) env)
 =>
(eval (let-expression) env)
=>
(eval (let->combination exp) env)
=>
(eval (application exp parameters) env)


|#