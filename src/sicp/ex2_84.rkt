#lang racket

(define (type-height type)
  (cond 
    ((eq? type 'integer) 0)
    ((eq? type 'rational) 1)
    ((eq? type 'real) 2)
    ((eq? type 'complex) 3)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x) (lambda (x) x))

(define (raise-to type1 type2)
  (if (eq? type1 type2)
      identity
      (let ((t1->upper (get 'raise (list type1))))
        (compose t1->upper (raise-to t1->upper type2)))))

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
                (let ((th1 (type-height type1))
                      (th2 (type-height type2)))
                  (if (> th1 th2)
                      (let ((t2->t1 (raise-to type2 type1)))
                        (if (t2->t1)
                            (apply-generic op a1 (t2->t1 a2))
                            (error "cannot coerce type2 to type1")))
                      (let ((t1->t2 (raise-to type1 type2)))
                        (if (t1->t2)
                            (apply-generic op (t1->t2 a1) a2)
                            (error "cannot coerce type1 to type2")))))))))))
