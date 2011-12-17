#lang racket

(require "ch2_3_3_sets_as_trees.rkt")

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; part a. Yes, both of them give the same results.

(define l1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define l2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define l3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 l1)
(tree->list-1 l2)
(tree->list-1 l3)

(tree->list-2 l1)
(tree->list-2 l2)
(tree->list-2 l3)

(provide tree->list-1 tree->list-2)

;; part b.
#|
for tree->list-1, it does a cons and an append at each step. append needs to be done
for each element of the first list. In this case the first list initially will have
n/2 elements, then n/4 and so on. So, it has about O(logn) steps. Now, it also does
a cons on each element, so overall it takes O(n Log n) steps.

for tree->list-2, it does a cons on each element, so steps is O(n).

Order of growth for both the procedures is O(n).
|#