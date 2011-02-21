#lang r5rs

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (assoc-in key-list records)
  (if (not (null? key-list))
      (if (not (null? (cdr key-list)))
          (assoc-in (cdr key-list) 
                    (assoc (car key-list) (cdr records)))
          (assoc (car key-list) (cdr records)))
      #f))

(define (lookup-in key-list records)
  (let ((record (assoc-in key-list records)))
    (if record
        (cdr record)
        #f)))

(define (form-table keys value)
  (cond 
    ((null? (cdr keys)) (cons (car keys) value))
    (else (list (car keys)
                (form-table (cdr keys) value)))))

(define (insert-in! key-list value records)
  (let loop ((k (car key-list))
             (ks (cdr key-list))
             (table (cdr records)))
    (let ((record (assoc k table)))
      (if record
          (if (null? ks)
              (set-cdr! record value)
              (loop (car ks) (cdr ks) (cdr record)))
          (if (null? ks)
              (set-cdr! table 
                        (cons (cons k value)
                              (cdr table)))
              (set-cdr! records 
                        (cons (list k
                                    (form-table ks value))
                              (cdr records))))))))
                          
        
          
        

#|
> (define t (list '*table*
                  (list 'math
                        (cons '+ 43)
                        (cons '- 45)
                        (cons '* 42))
                  (list 'letters
                        (cons 'a 97)
                        (cons 'b 98))))
                         
> (lookup-in '(math +) t)
43
> (lookup-in '(math /) t)
#f
> (lookup-in '(letters /) t)
#f
> (lookup-in '(letters a) t)
97
> 
|#

#|
> (define t (list '*table*
                  (list 'math
                        (cons '+ 43)
                        (cons '- 45)
                        (cons '* 42))
                  (list 'letters
                        (cons 'a 97)
                        (cons 'b 98))))
> (display t)
(*table* (math (+ . 43) (- . 45) (* . 42)) (letters (a . 97) (b . 98)))
> (insert-in! '(earth asia india) 'delhi t)
> (display t)
(*table* (earth (asia (india . delhi))) (math (+ . 43) (- . 45) (* . 42)) (letters (a . 97) (b . 98)))
> (insert-in! '(math +) 83 t)
> (display t)
(*table* (earth (asia (india . delhi))) (math (+ . 83) (- . 45) (* . 42)) (letters (a . 97) (b . 98)))
> (insert-in! '(math /) 99 t)
> (display t)
(*table* (earth (asia (india . delhi))) (math (+ . 83) (/ . 99) (- . 45) (* . 42)) (letters (a . 97) (b . 98)))
> 
|#
