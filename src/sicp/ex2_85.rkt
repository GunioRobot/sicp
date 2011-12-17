#lang racket

(put 'project '(complex)
     (lambda (c) (real c)))

(put 'project '(rational)
     (lambda (r) (floor r)))

(put 'project '(real)
     (lambda (d)
       (let ((rat (rationalize (inexact->exact d) 1/100)))
         (make-rat (numerator rat)
                   (denominator rat)))))

;; drop
(define (drop x)
  (let ((project-fn (get 'project (list (type-tag x)))))
    (if (project-fn)
        (let ((dropped-x (project-fn x)))
          (if (equ? (raise dropped-x) x)
              (drop dropped-x)
              x))
        x)))


