#lang racket

(define (map1 f list)
  (cond
    [(empty? list) '()]
    [else (cons (f (first list))
                (map1 f (rest list)))]))

(define (mymap f lists)
  (cond
    [(empty? (first lists)) '()]
    [else (cons (apply f (map1 first lists))
                (mymap f (map1 rest lists)))]))

;; 2.28
(define (fringe coll)
  (cond
    [(null? coll) '()]
    [(not (pair? coll)) (list coll)]
    [else (append (fringe (first coll))
                  (fringe (rest coll)))]))

(define (scale-tree tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree subtree factor)
             (* subtree factor)))
       tree))

;; 2.30
(define (square-tree1 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree1 subtree)
             (* subtree subtree)))
       tree))

(define (square-tree2 tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (* tree tree)]
    [else (cons (square-tree2 (car tree))
                (square-tree2 (cdr tree)))]))

(define (fringe1 tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             subtree
             (fringe1 subtree)))
       tree))


(define (tree-map f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map f subtree)
             (f subtree)))
       tree))
