#lang racket

(define (install-dense-termlist-package)
  ;; internal procedures
  (define (adjoin-term term term-list) 
    (cons (coeff term) term-list))
  (define (first-term term-list)
    (let ((len (length term-list)))
      (make-term (- len 1) (car term-list))))
  (define (rest-terms term-list) (cdr term-list))
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'dense x))  
  (put 'adjoin-term '(term dense)
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) 
       (lambda (term-list) 
         (tag (rest-terms term-list))))
  (put 'the-empty-termlist '(dense)
       (lambda ()
         (tag (the-empty-termlist))))
  (put 'empty-termlist? '(dense) 
       (lambda (tl)
         (empty-termlist? (contents tl))))
  'done)

(define (install-sparse-termlist-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons (coeff term) term-list)))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (the-empty-termlist) '())  
  (define (empty-termlist? term-list) (null? term-list))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'sparse x))
  (put 'adjoin-term '(term sparse) 
       (lambda (term term-list) 
         (tag (adjoin-term term term-list))))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) 
       (lambda (term-list)
         (tag (rest-terms term-list))))
  (put 'the-empty-termlist '(sparse)
       (lambda ()
         (tag (the-empty-termlist))))
  (put 'empty-termlist? '(sparse) 
       (lambda (tl)
         (empty-termlist? (contents tl))))
  'done)

(define (install-term-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (tag x) (attach-tag 'term x))
  (put 'make-term '(scheme-number scheme-number)
       (lambda (o c)
         (tag (make-term o c))))
  (put 'order '(term) 
       (lambda (t)
         (order (contents t))))
  (put 'coeff '(term)
       (lambda (t)
         (coeff (contents t))))
  'done)

;; generic procedures
(define (adjoin-term term terms) (apply-generic 'adjoin-term term terms))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (empty-termlist? tl) (apply-generic 'empty-termlist? tl))

(define (make-term order coeff) (apply-generic 'make-term order coeff))
(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))

#|

the-empty-termlist needs special treatment because it has no input. This can be remedied
by get'ing the right empty term list using:

 (get 'the-empty-termlist '(sparse))

or 

 (get 'the-empty-termlist '(dense))

|#
