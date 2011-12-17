(ns sicp.ex2_54
  (:use [clojure.test]))

(defn atom? [x]
  (not (list? x)))

(defn eq? [x y]
  (if (not (atom? x))
    false
    (= x y)))

(defn equal? [a b]
  (cond (and (atom? a) (atom? b)) (eq? a b)
        (and (not (atom? a)) (not (atom? b))) (and (empty? a) (empty? b))
        (atom? (first a))
        (if (eq? (first a) (first b))
          (equal? (rest a) (rest b))
          false)
        :else (and (equal? (first a) (first b))
                   (equal? (rest a) (rest b)))))

(deftest test-equality
  (are [x y] [= x y]
       (equal? '(1) '(1)) true
       (equal? '(1) '(2)) false
       (equal? '(1 2) '(1 2)) true
       (equal? '(1 2 (3)) '(1 2 (3))) true
       (equal? '(1 2 3) '(1 2 (3))) false))