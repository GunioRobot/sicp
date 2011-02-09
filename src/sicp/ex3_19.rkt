#lang r5rs

(define (contains-cycle? x y)
  (if (or (not (pair? x))
          (not (pair? y)))
      #f
      (let ((t (cdr x))
            (h (cdr (cdr y))))
        (if (eqv? t h)
            #t
            (contains-cycle? t h)))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
(define z1 (list 'a 'b 'c))

(contains-cycle? z (cdr z))
(contains-cycle? z1 (cdr z1))