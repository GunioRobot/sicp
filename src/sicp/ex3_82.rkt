#lang racket

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (point-stream x1 x2 y1 y2)
  (cons-stream
   (list (random-in-range x1 x2)
         (random-in-range y1 y2))
   (point-stream x1 x2 y1 y2)))

(define (P x y)
  (define (inside-circle? radius centre-x centre-y)
    (<= (+ (square (- x centre-x))
           (square (- y centre-y)))
        (square radius)))
  (inside-circle? 1.0 0 0))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral P x1 x2 y1 y2)
  (monte-carlo (stream-map (lambda (p)
                             (apply P p))
                           (point-stream x1 x2 y1 y2))
               0
               0))

(* (stream-ref (estimate-integral P 1 -1 1 -1) 100000) 4.0)
