#lang r5rs

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; list of exactly 3 pairs which counts as 3
(define z1 (cons 'a (cons 'b (cons 'c '()))))
(count-pairs z1)

;; list of 3 pairs but counts as 4
(define x (list 'a 'b))
(define z2 (cons (cons 'c '()) x))
(count-pairs z2)

;; list of 3 pairs but counts as 7
(define x3 (cons 'a 'b))
(define y3 (cons x3 x3))
(define z3 (cons y3 y3))
(count-pairs z3)

;; list of 3 pairs but count never returns
;;; DON'T RUN THIS. IT WILL GO INTO An INF LOOP
(define z4 (cons 'a (cons 'y (cons 'z '()))))
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(set-cdr! (last-pair z4) z4)
(count-pairs z4)
