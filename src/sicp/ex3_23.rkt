#lang r5rs

(define error display)

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (make-cell item)
  (cons item (cons '() '())))

(define (value cell)
  (car cell))

(define (next-cell cell)
  (car (cdr cell)))

(define (prev-cell cell)
  (cdr (cdr cell)))

(define (set-next! cell pair)
  (set-car! (cdr cell) pair))

(define (set-prev! cell pair)
  (set-cdr! (cdr cell) pair))

(define (front-insert-deque! deque item)
  (let ((new-pair (make-cell item)))
    (cond 
      ((empty-deque? deque)
       (set-front-ptr! deque new-pair)
       (set-rear-ptr! deque new-pair)
       deque)
      (else
       (set-next! new-pair (front-ptr deque))
       ;(set-cdr! (cdr (front-ptr deque)) new-pair)
       (set-prev! (front-ptr deque) new-pair)
       (set-front-ptr! deque new-pair)
       deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (make-cell item)))
    (cond
      ((empty-deque? deque)
       (set-front-ptr! deque new-pair)
       (set-rear-ptr! deque new-pair)
       deque)
      (else
       (set-prev! new-pair (rear-ptr deque))
       ;(set-car! (cdr (rear-ptr deque)) new-pair)
       (set-next! (rear-ptr deque) new-pair)
       (set-rear-ptr! deque new-pair)
       deque))))

(define (front-delete-deque! deque)
  (cond
    ((empty-deque? deque)
     (error "DELETE! called with an empty deque"))
    (else
     (set-front-ptr! deque (car (cdr (front-ptr deque))))
     deque)))
     
(define (rear-delete-deque! deque)
  (cond
    ((empty-deque? deque)
     (error "DELETE! called with an empty deque"))
    (else
     (set-rear-ptr! deque (prev-cell (rear-ptr deque)))
     (set-next! (rear-ptr deque) '())
     deque)))
     

(define (print-deque queue)
  (define (copy-queue q1)
    (let ((q2 (make-deque)))
      (set-front-ptr! q2 (front-ptr q1))
      (set-rear-ptr! q2 (rear-ptr q1))
      q2))
  (let ((q (copy-queue queue)))
    (if (not (empty-deque? q))
        (begin
          (display (front-deque q))
          (display " ")
          (print-deque (front-delete-deque! q))))))

#|
> (define q (make-deque))
> (front-insert-deque! q 'a)
(mcons (mcons 'a (mcons '() '())) (mcons 'a (mcons '() '())))
> (front-insert-deque! q 'b)
(mcons #0=(mcons 'b (mcons #1=(mcons 'a (mcons '() #0#)) '())) #1#)
> (front-insert-deque! q 'c)
(mcons
 #0=(mcons 'c (mcons #1=(mcons 'b (mcons #2=(mcons 'a (mcons '() #1#)) #0#)) '()))
 #2#)
> (print-deque q)
c b a 
> q
(mcons
 #0=(mcons 'c (mcons #1=(mcons 'b (mcons #2=(mcons 'a (mcons '() #1#)) #0#)) '()))
 #2#)
> (rear-delete-deque! q)
(mcons #0=(mcons 'c (mcons #1=(mcons 'b (mcons '() #0#)) '())) #1#)
> (print-deque q)
c b 
> (rear-insert-deque! q 'd)
(mcons
 #0=(mcons 'c (mcons #1=(mcons 'b (mcons #2=(mcons 'd (mcons '() #1#)) #0#)) '()))
 #2#)
> (print-deque q)
c b d 
> (front-insert-deque! q 'e)
(mcons
 #0=(mcons
     'e
     (mcons
      #1=(mcons
          'c
          (mcons #2=(mcons 'b (mcons #3=(mcons 'd (mcons '() #2#)) #1#)) #0#))
      '()))
 #3#)
> (print-deque q)
e c b d 
> (front-delete-deque! q)
(mcons
 #0=(mcons
     'c
     (mcons
      #1=(mcons 'b (mcons #2=(mcons 'd (mcons '() #1#)) #0#))
      (mcons 'e (mcons #0# '()))))
 #2#)
> (print-deque q)
c b d 
> 
|#