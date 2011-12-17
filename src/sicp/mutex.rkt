#lang racket

(define (make-serializer)
  (let ([mutex (make-mutex)])
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ([val (apply p args)])
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ([cell (mcons #f '())])
    (define (the-mutex m)
      (cond [(eq? m 'acquire)
             (when (test-and-set! cell)
               (the-mutex 'acquire))] ;; loop till we acquire the mutex
            [(eq? m 'release) (clear! cell)]))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell #f))

(define (test-and-set! cell)
  (if (mcar cell)
      #t
      (begin (set-mcar! cell #t)
             #f)))
