#lang r5rs

(define (make-entry k v) (cons k v))
(define (key entry) (car entry))
(define (value entry) (cdr entry))
(define (set-key! entry k) (set-car! entry k))
(define (set-value! entry v) (set-cdr! entry v))
(define (null-entry? e)
  (if (null? e)
      #t
      (or (null? (key e))
          (null? (value e)))))

;(define (set-entry! k v e)
;  (set-key! k e)
;  (set-value! v e))

(define (make-node entry left right)
  (cons entry (cons left (cons right '()))))

(define (entry-node node) (car node))
(define (left-node node) (car (cdr node)))
(define (right-node node) (car (cdr (cdr node))))

(define (make-tree)
  (make-node (make-entry '() '()) '() '()))

(define (entry-tree tree)
  (entry-node tree))

(define (left-branch tree)
  (left-node tree))

(define (right-branch tree)
  (right-node tree))

(define (set-left-branch! tree val)
  (set-car! (cdr tree) val))

(define (set-right-branch! tree val)
  (set-car! (cdr (cdr tree)) val))

(define (set-entry! tree entry)
  (set-car! tree entry))

(define (make-tree-node k v)
  (make-node (make-entry k v) '() '()))

(define (lookup k table)
  (let ((e (entry-tree table)))
    (cond
      ((null-entry? e) #f)
      ((= k (key e)) (value e))
      ((< k (key e)) (lookup k (left-branch table)))
      ((> k (key e)) (lookup k (right-branch table))))))

(define (insert! k v table)
  (cond
    ((null-entry? (entry-tree table)) (set-entry! table (make-entry k v)))
    ((= k (key (entry-tree table))) (set-value! (entry-tree table) v))
    ((< k (key (entry-tree table)))
     (if (null? (left-branch table))
         (set-left-branch! table (make-tree-node k v))
         (insert! k v (left-branch table))))
    ((> k (key (entry-tree table)))
     (if (null? (right-branch table))
         (set-right-branch! table (make-tree-node k v))
         (insert! k v (right-branch table)))))
'ok)