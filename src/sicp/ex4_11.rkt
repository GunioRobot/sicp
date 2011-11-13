#lang racket

(define (make-frame variables values)
  (if (not (= (length variables)
              (length values)))
      (error "Number of variables and values should be the same -- MAKE-FRAME")
      (map cons variables values)))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))
