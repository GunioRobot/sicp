(ns sicp.ex2_65
  (:use [sicp.ex2_63]
        [sicp.ex2_64]
        [sicp.ex2_62 :only (union-set)]
        [sicp.ch2_3  :only (intersection-set)]))

(defn union-set-tree [set1 set2]
  (let [ls1 (tree->list-1 set1)
        ls2 (tree->list-1 set2)]
    (list->tree (union-set ls1 ls2))))


(defn intersection-set-tree [set1 set2]
  (let [ls1 (tree->list-1 set1)
        ls2 (tree->list-1 set2)]
    (list->tree (intersection-set ls1 ls2))))