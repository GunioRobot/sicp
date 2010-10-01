#lang racket
#|
magnitude is defined in section 2.4.3 as:

(define (magnitude z) (apply-generic 'magnitude z))

In this example z looks as '(complex rectangular 3 . 4))

The apply-generic procedure is defined as follows:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

Here, we find the type-tag of z, in this case, it will be 'complex.
The table constructed with the generic prodecure "magnitude" was using
the type of z, namely 'rectangular. 

The remedy as suggested by Alyssa is as follows:

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

Now, this will add a new column for each of the operations (real-part, imag-part,
magnitude and angle) for the type '(complex) and install the corresponding function.

Now, when we do (type-tag z), we get 'complex and then we look for the type-specific
procedure using (get op type-tag) which will give the right procedure.

(apply-generic 'magnitude z)

=> (apply-generic 'magnitude '(complex rectangular x . y)

=> ((get 'magnitude 'complex) '(rectangular x . y) 

=> (magnitude '(rectangular x . y))

=> (apply-generic 'magnitude '(rectangular x . y))

=> ((get 'magnitude 'rectangular) '(x . y))

So, apply-generic gets invoked twice.


(

|#
