#lang racket

;; left to right
(define (list-of-values exps env)
  (define (iter valuelist expressions)
    (if (no-operands? exps)
        valuelist
        (let ((value (eval (first-operand expressions) env)))
          (iter (append valuelist (list value))
                (rest-operands expressions)))))
  (iter '() exps))

;; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-exps exps) env)))
        (cons (eval (first-exp exps) env)
              right))))
