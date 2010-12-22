#lang racket

(require "ch2_3_3_sets_as_trees.rkt")
(require "ex2_63.rkt")
(require "ex2_64.rkt")
(require "ex2_62.rkt")

(define (union-set-tree s1 s2)
  (let ((ls1 (tree->list-1 s1))
        (ls2 (tree->list-2 s2)))
    (list->tree (union-set ls1 ls2))))
