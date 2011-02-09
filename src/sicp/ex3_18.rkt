#lang r5rs

#|
we use a unique hash and store them into a set. If we again hit an element
with the same hash, we have hit a cycle. For simplicity, we use car of a cell
as the hash. So for this procedure to work, we will need unique values in each
cell.
|#

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (cycle? x)
  (define (contains-cycle? x trail)
    (let ((f (car x))
          (n (cdr x)))
      (cond
        ((not (pair? n)) #f)
        ((memq f trail)  #t)
        (else (contains-cycle? (cdr x) (cons f trail))))))
  (contains-cycle? x '()))
