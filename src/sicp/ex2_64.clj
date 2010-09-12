(ns sicp.ex2_64
  (:use [sicp.ch2_3 :only (make-tree)]))

(declare partial-tree)

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(defn partial-tree [elts n]
  (if (= n 0)
    (cons '() elts)
    (let [left-size     (quot (- n 1) 2)
          left-result   (partial-tree elts left-size)
          left-tree     (first left-result)
          non-left-elts (rest left-result)
          right-size    (- n (+ left-size 1))
          this-entry    (first non-left-elts)
          right-result  (partial-tree (rest non-left-elts) right-size)
          right-tree    (first right-result)
          remaining-elts (rest right-result)]
      (cons (make-tree this-entry
                       left-tree
                       right-tree)
            remaining-elts))))

(comment
  "partial-tree divides the input sequence roughly into 2 halfs and creates a tree of
   partial-trees on each of the branches."
  "The tree for the input '(1 3 5 7 9 11) looks like this:"
  "
          5
        /   \
       1     9
        \   / \
         3 7   11
  "
  "partial-tree does a cons for every element of the list. So, order of growth is O(n)."
)