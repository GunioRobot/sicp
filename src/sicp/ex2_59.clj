(ns sicp.ex2_59
  (:use [clojure.test]
        [sicp.ch2_3]))

(defn union-set [set1 set2]
  (cond (empty? set1) set2
        (element-of-set? (first set1) set2) (union-set (rest set1) set2)
        :else (cons (first set1) set2)))