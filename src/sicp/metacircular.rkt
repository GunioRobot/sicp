#lang racket

;; metacircular evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; =====
(define (self-evaluating? expr)
  (cond [(number? expr) #t]
        [(string? expr) #t]
        [else #f]))

(define (variable? expr)
  (symbol? expr))

;; quotation
(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (tagged-list? expr tag)
  (or (pair? expr) (eq? (car expr) tag)))

(define (text-of-quotation expr)
  (car (cdr expr)))

;; assignment
(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr)
  (car (cdr expr)))

(define (assignment-value expr)
  (car (cdr (cdr expr))))

;; define
(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (car (cdr expr))) ;; (define foo 42)
      (car (cdr expr))
      ;; (define (foo x) (....))
      (car (car (cdr expr)))))

(define (definition-value expr)
  ;; (define foo 42)
  (if (symbol? (car (cdr expr)))
      (car (cdr (cdr expr)))
      ;; (define (foo x) (....))
      (make-lambda (cdr (car (cdr expr))) ;; gives (x)
                   (cdr (cdr expr)))))    ;; body

;; lambda
(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-parameters expr)
  (car (cdr expr)))

;;(lambda (x) (..)(..)...(..))
(define (lambda-body expr)
  (cdr (cdr expr)))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate expr) (car (cdr expr)))
(define (if-consequent expr) (car (cdr (cdr expr))))
(define (if-alternative expr) 
  (if (not (null? (cdr (cdr (cdr expr)))))
      (car (cdr (cdr (cdr expr))))
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; procedure application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; expander for `cond' expressions to be transformed into `if' expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))