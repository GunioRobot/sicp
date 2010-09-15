#lang racket

(require "ch2_3.rkt"
         rackunit)

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-message (decode sample-bits sample-tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond 
    [(and (leaf? tree) 
          (eqv? (symbol-leaf tree) symbol)) '()]
    [(member? symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree)))]
    [(member? symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree)))]
    [(not (member? symbol (symbols tree))) (error "Symbol not in the tree -- ENCODE-SYMBOL " symbol)]))

(define (member? x set)
  (cond
    [(null? set) #f]
    [(eqv? x (car set)) #t]
    [else (member? x (cdr set))]))

;;; a simple test. encode-decode should give me back the same symbols.
(check equal? 
       (decode (encode sample-message 
                       sample-tree) 
               sample-tree) 
       sample-message)
  