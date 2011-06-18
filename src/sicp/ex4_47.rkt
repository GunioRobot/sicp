#lang racket

;; semaphore implementation using mutex
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

;; semaphore implementation
(define (make-semaphore n)
  (let ([cell 0]
        [mutex (make-mutex)])
    (define (the-semaphore s)
      (cond [(eq? s 'acquire)
             (mutex 'acquire)
             (if (>= (+ cell 1) n)
                 (begin
                   (mutex 'release)
                   (the-semaphore 'acquire))
                 (begin
                   (set! cell (+ cell 1))
                   (mutex 'release)))]
            [(eq? s 'release)
             (mutex 'acquire)
             (when (> cell 0)             
               (set! cell (- cell 1)))
             (mutex 'release)]))
    the-semaphore))

;; using test-and-set!
(define (make-semaphore n)
  (let ([cell 0]
        [flag #f])
    (define (the-semaphore s)
      (cond [(eq? s 'acquire)
             (if (test-and-set! flag)
                 (the-semaphore 'acquire))
             (if (>= (+ cell 1) n)
                 (begin
                   (clear! flag)
                   (the-semaphore 'acquire))
                 (begin
                   (set! cell (+ cell 1))
                   (clear flag)))]
            [(eq? s 'release)
             (if (test-and-set! flag)
                 (the-semaphore 'acquire))
             (when (> cell 0)             
               (set! cell (- cell 1)))
             (clear! flag)]))
    the-semaphore))
