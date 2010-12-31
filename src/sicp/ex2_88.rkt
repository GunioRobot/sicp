#lang racket

(define (negate x) (- x))

(put 'negate '(scheme-number)
     (lambda (r) (tag (negate r))))

(put 'negate '(rational)
     (lambda (r) (tag (negate r))))

(put 'negate '(real)
     (lambda (r) (tag (negate r))))

(put 'negate '(complex)
     (lambda (c) (tag ((get 'make 'complex) (negate (real c))
                                            (negate (imag c))))))

(put 'negate '(polynomial)
     (lambda (p) (tag (negate-poly p))))

(define (negate-terms terms)
  (if (empty-termlist? terms)
      the-empty-termlist
      (let ((t1 (first-term terms)))
        (let ((o (order t1))
              (c (coeff t1)))
          (adjoin-term (make-term o (negate c))
                       (negate-terms (rest-terms terms)))))))

(define (negate-poly p)
  (let ((terms (term-list p)))
    (make-poly (variable p) (negate-terms terms))))

(put 'negate '(polynomial)
     (lambda (p) (tag (negate-poly p))))

(define (negate p) (apply-generic 'negate p))

(define (sub-poly p1 p2)
  (add-poly p1 (negate p2)))

(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (sub-poly p1 p2))))