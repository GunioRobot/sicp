#lang racket/load

(require "utils.rkt")


; section 2.4
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;; constructors and selectors for rectangular form
(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))

(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z))
           (square (imag-part-rect z)))))

(define (angle-rect z)
  (atan (imag-part-rect z) (real-part-rect z)))

(define (make-from-real-imag-rect x y) (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rect r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

;;; constructors and selectors for polar form
(define (real-part-polar z) (* (magnitude-polar z) 
                               (cos (angle-polar z))))

(define (imag-part-polar z) (* (magnitude-polar z) 
                               (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z)     (cdr z))

(define (make-from-real-imag x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang r a) (attach-tag 'polar (cons r a)))

;; type tag
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; generic procedures
(define (real-part z)
  (cond
    [(rectangular? z) (real-part-rect (contents z))]
    [(polar? z)       (real-part-polar (contents z))]
    [else (error "unknown type -- real-part" z)]))

(define (imag-part z)
  (cond
    [(rectangular? z) (imag-part-rect (contents z))]
    [(polar? z)       (imag-part-rect (contents z))]
    [else (error "unknown type -- imag part" z)]))

(define (magnitude z)
  (cond
    [(rectangular? z) (magnitude-rect (contents z))]
    [(polar? z)       (magnitude-polar (contents z))]
    [else (error "Unknown type -- MAGNITUDE" z)]))

(define (angle z)
  (cond [(rectangular? z) (angle-rect (contents z))]
        [(polar? z)       (angle-polar (contents z))]
        [else (error "Unknown type -- ANGLE" z)]))

(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;; message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond
      [(eq? op 'real-part) x]
      [(eq? op 'imag-part) y]
      [(eq? op 'magnitude)
       (sqrt (+ (square x) (square y)))]
      [(eq? op 'angle) (atan y x)]
      [else 
       (error "unknown op" op)]))
  dispatch)

