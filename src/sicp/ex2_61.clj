(ns sicp.ex2_61
  (:use [sicp.ex2_54 :only (equal? eq?)]
        [clojure.test]))

(defn adjoin-set [x set]
  (let [x1 (first set)]
    (cond (empty? set) (cons x set)
          (= x x1) set
          (< x x1) (cons x set)          
          (> x x1) (cons x1 (adjoin-set x (rest set))))))

(deftest test-adjoin
  (are [x y] [= x y]
       (adjoin-set 0 '())    '(0)
       (adjoin-set 1 '(2 3)) '(1 2 3)
       (adjoin-set 3 '(1 2 3)) '(1 2 3)
       (adjoin-set 3 '(1 2 4)) '(1 2 3 4)
       (adjoin-set 4 '(1 2 4)) '(1 2 4)
       (adjoin-set 4 '(1 2 3)) '(1 2 3 4)))