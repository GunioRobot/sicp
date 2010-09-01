(ns sicp.ex2_46
  (:use [clojure.test]))

(defn make-vect [x y]
  (list x y))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (second v))

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(defn scale-vect [v s]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(deftest test-add-vectors
  (let [v1 (make-vect 1 1)
        v2 (make-vect 2 2)]
    (is (= (add-vect v1 v2) (make-vect 3 3)))))
