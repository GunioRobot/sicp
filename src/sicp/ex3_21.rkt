#lang r5rs

(define error display)

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue)
      (else
       (set-cdr! (rear-ptr queue) new-pair)
       (set-rear-ptr! queue new-pair)
       queue))))
(define (delete-queue! queue)
  (cond
    ((empty-queue? queue)
     (error "DELETE! called on an empty queue" queue))
    (else
     (set-front-ptr! queue (cdr (front-ptr queue)))
     queue)))

(define (print-queue queue)
  (define (copy-queue q1)
    (let ((q2 (make-queue)))
      (set-front-ptr! q2 (front-ptr q1))
      (set-rear-ptr! q2 (rear-ptr q1))
      q2))
  (let ((q (copy-queue queue)))
    (if (not (empty-queue? q))
        (begin
          (display (front-queue q))
          (display " ")
          (print-queue (delete-queue! q))))))


