#lang racket

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms (sub-terms L1
                                           (mul-term-by-all-terms (make-term new-o new-c)
                                                                  L2))
                                L2)))
                (list (adjoin-term (make-term new-o new-c) 
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (div-poly P1 P2)
  (if (same-variable? (variable P1)
                      (variable P2))
      (let ((t1 (term-list P1))
            (t2 (term-list P2)))
        (let ((div-results (div-terms t1 t2)))
          (list (make-poly (variable P1) (car div-results))
                (make-poly (variable P1) (cadr div-results)))))
      (error "Polynomials are not of the same variable -- DIV-POLY")))