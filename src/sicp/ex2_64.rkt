#lang racket

(require "ch2_3_3_sets_as_trees.rkt")
(require racket/trace)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-results (partial-tree elts left-size)))
          (let ((left-tree (car left-results))
                (non-left-elements (cdr left-results))
                (right-size (- n (+ left-size 1))))
            (let ((this (car non-left-elements))
                  (right-results (partial-tree (cdr non-left-elements) right-size)))
              (let ((right-tree (car right-results))
                    (remaining-elts (cdr right-results)))
                (cons (make-tree this left-tree right-tree)
                      remaining-elts))))))))

;;(trace list->tree)
;;(trace partial-tree)

(provide list->tree)

;; a
#|
Let us take an example:

  (list->tree '(1 3 5 7 9 11))

We do 
(partial-tree '(1 3 5 7 9 11) 6) 
  => (partial-tree '(1 3 5 7 9 11) 2)
    => (partial-tree '(1 3 5 7 9 11) 0)

At this point, we have found a leaf node. So, we push the null list,
take the first element on the list as the parent and look for the 
right tree. The call returns to the previous call to partial-tree
(i.e. (partial-tree '(1 3 5 7 9 11) 2)) which means n is 2.

The right-size = (- n (+ left-size 1)) => (- 2 (+ 0 1)) => 1
So we invoke partial-tree for the right tree.

(partial-tree '(3 5 7 9 11) 1)
  => (partial-tree '(3 5 7 9 11) 0)

This will push an empty list (as the left node) and look for right node.
(partial-tree '(5 7 9 11) 0)
 
this gives us the subtree (1 () (3 () ()))

At every stage of making a subtree, we cons it with the remaining elements.
The tree looks like this:

               5
              / \
             /   \
            1     9
           / \    /\
          /   \  7  11
        ()     3
              / \
             /   \
            ()   ()

part b. For each node, we do a cons of the tree under the node with the remaining
elements. So the order of growth is O(n).

|#
                
          