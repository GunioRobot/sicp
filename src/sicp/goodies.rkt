#lang racket
(provide distinct)

(define (member? x lst)
  (cond
    [(empty? lst) #f]
    [(equal? (first lst) x) #t]
    [else (member? x (rest lst))]))

(define (distinct lst)
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (distinct (rest lst))]
    [else (cons (first lst) 
                (distinct (rest lst)))]))

                  