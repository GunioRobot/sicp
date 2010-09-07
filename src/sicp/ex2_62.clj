(ns sicp.ex2_62
  (:use [clojure.test]))

(defn union-set [set1 set2]
  (let [x1 (first set1)
        x2 (first set2)]
    (cond (empty? set1) set2
          (empty? set2) set1
          (= x1 x2) (cons x1 (union-set (rest set1)
                                        (rest set2)))
          (< x1 x2) (cons x1 (union-set (rest set1)
                                        set2))
          (> x1 x2) (cons x2 (union-set set1
                                        (rest set2))))))

(deftest test-union
  (are [x y] [= x y]
       (union-set '() '()) '()
       (union-set '(1) '()) '(1)
       (union-set '() '(1)) '(1)
       (union-set '(1 2 3) '(1 2 3)) '(1 2 3)
       (union-set '(1 2 3 4) '(1 2 3)) '(1 2 3 4)
       (union-set '(1 2 4) '(3 4 5)) '(1 2 3 4 5)
       (union-set '(1 2 3 4) '(5 6 7 8)) '(1 2 3 4 5 6 7 8)
       (union-set '(1 2 3) '(3 4 5 6)) '(1 2 3 4 5 6)))
