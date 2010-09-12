(ns sicp.ex2_63
  (:use [sicp.ch2_3 :only (entry left-branch right-branch)]
        [clojure.test]))

(defn tree->list-1 [tree]
  (if (empty? tree)
    '()
    (concat (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(defn tree->list-2 [tree]
  (let [copy-to-list (fn f [tree result-list]
                       (if (empty? tree)
                         result-list
                         (f (left-branch tree)
                            (cons (entry tree)
                                  (f (right-branch tree) result-list)))))]
    (copy-to-list tree '())))

(deftest test-for-some-trees
  (are [x y] [= x y]
       (tree->list-1 '(7 (3 (1 ()) (5 ())) (9 () (11 ())))) '(1 3 5 7 9 11)
       (tree->list-2 '(7 (3 (1 ()) (5 ())) (9 () (11 ())))) '(1 3 5 7 9 11)
       (tree->list-1 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) '(1 3 5 7 9 11)
       (tree->list-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) '(1 3 5 7 9 11)
       (tree->list-1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))) '(1 3 5 7 9 11)
       (tree->list-2 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))) '(1 3 5 7 9 11)))

;;;; part a
(comment
  "yes, as the above tests show, they add elements into the list in the same order
   and hence produce same lists."
)

;;;; part b
(comment
  "tree->list-1 does a cons and an append of two lists at every step. The depth of the
   tree is log(n). append grows as length of the list. The number of steps required to
   traverse down to the tree is log(n). So, this is a O(n.log(n)) operation."

  "list->tree-2 does only a cons operation at every step. So this is a O(n) operation."
)
