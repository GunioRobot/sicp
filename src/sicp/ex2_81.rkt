#lang racket

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt

#|

2.81 a. The initial (get 'exp '(complex complex)) yield a false as the procedure
to handle '(complex complex) was not installed into the table. So, it gets the coercion
table:

 (get-coersion 'complex 'complex) => complex->complex 

 (apply-generic 'exp (complex->complex a1) a2) => (apply-generic 'exp a1 a2)

So, we keep calling ourselves until stack overflows.
 
|#

#|

b.

As long as we install a procedure to handle same typed data in the main table and not
do a same type to same type coercion, it is fine. So, Louis is wrong.

|#

;; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond 
        (proc (apply proc (map contents args)))
        ((and (car type-tags) (cadr type-tags))
         (error "No procedure to handle the type" (car type-tags)))
        ((= (length args) 2)
         (let ((type1 (car type-tags))
               (type2 (cadr type-tags))
               (a1 (car args))
               (a2 (cadr args)))
           (let ((t1->t2 (get-coercion type1 type2))
                 (t2->t1 (get-coercion type2 type1)))
             (cond (t1->t2
                    (apply-generic op (t1->t2 a1) a2))
                   (t2->t1
                    (apply-generic op a1 (t2->t1 a2)))
                   (else
                    (error "No method for these types"
                           (list op type-tags)))))))
        (else (error "No method for these types"
                     (list op type-tags)))))))