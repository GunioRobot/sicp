#lang racket

;; get a list of coercions (t1->t t2->2 .... tn->t)
;; if a type coercion does not exist, the
;; particular index will have #f
(define (get-coercions type types)
  (map (lambda (t)
         (if (eq? t type)
             (lambda (x) x)
             (get-coercion t type)))
       types))

(define (all-valid? coercions)
  (cond
    ((null? coercions) #t)
    ((car coercions) (all-valid? (cdr coercions)))
    (else #f)))

(define (get-all-type-coercions types)
  (map (lambda (t)
         (get-coercions t types))
       types))

(define (apply-generic op . args)
  (define (apply-generic-2 type-coercion-list)
    (cond
      ((null? type-coercion-list) (error "cannot find a suitable type coercion"))
      ((all-valid? (car type-coercion-list))
       (let ((coerced-args (map (lambda (t a) (t a)) (car type-coercion-list) args)))
         (apply-generic-1 coerced-args)))
      (else (apply-generic-2 (cdr type-coercion-list)))))

  (define (apply-generic-1 args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (let ((tn->t1 (get-all-type-coercions types)))
              (apply-generic-2 tn->t1))))))

  (apply-generic-1 args))

#|
The method will fail if say, t2, t3 and t4 can only be coreced into t1 but the call to apply-generic does not have an argument of type t1.
Instead, if we have a way to figure out the relation between types and the hierarchy, then we can deal with it better.
|#