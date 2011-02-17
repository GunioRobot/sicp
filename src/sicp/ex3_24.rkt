#lang r5rs

(define error display)

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond 
        ((null? records) #f)
        ((same-key? key (car (car records))) (car records))
        (else (assoc key (cdr records)))))
    
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table 
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE"))))
    dispatch))

;(define operation-table (make-table equal?))
;(define get (operation-table 'lookup-proc))
;(define put (operation-table 'insert-proc!))

#|
> (define numeric-table 
    (make-table (lambda (key-1 key-2) (< (abs (- key-2 key-1)) 2))))
> (define get-val (numeric-table 'lookup-proc))
> (define put-val (numeric-table 'insert-proc!))
> (put-val '1 1 1)
'ok
> (put-val '4 4 16)
'ok
> (get-val '4 4)
16
> (get-val '5 4)
16
> 
|#