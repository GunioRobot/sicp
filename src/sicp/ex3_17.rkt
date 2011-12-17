#lang r5rs

(define (traverse-tree! x)
  (cond
    ((not (pair? x)) x)
    ((eqv? (car x) 'traversed)
     (cons (traverse-tree! (car (cdr x)))
           (traverse-tree! (cdr (cdr x)))))
    (else
     (begin
       (set! x (cons 'traversed (cons (car x) (cdr x))))
       (traverse-tree! x)))))

(define (count-tree x)
  (cond
    ((not (pair? x)) 0)
    ((traversed? x) (+ (count-tree (left x))
                       (count-tree (right x))))
    (else
     (begin
       (set-car! x (traverse x))
       (+ (count-tree (left x))
          (count-tree (right x))
          1)))))

(define (traversed? x)
  (if (pair? (car x))
      (eqv? (car (car x)) 'traversed)
      #f))

(define (left x)
  (if (traversed? x)
      (cdr (car x))
      (car x)))

(define (right x)
  (cdr x))

(define (traverse x)
  (cons 'traversed
        (car x)))

(define x (list 'a 'b))
(define z (cons x x))
