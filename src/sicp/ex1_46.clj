(ns sicp.ex1_46
  (:use [clojure.contrib test-is]
	[sicp utils]))

(defn iterative-improve [good-enough-fn? improve-guess-fn]
  (fn [guess]
    (let [new-guess (improve-guess-fn guess)]
      (if (good-enough-fn? guess new-guess)
	new-guess
	(recur new-guess)))))

(defn sqrt [x]
  (let [initial-guess 1.0]
    ((iterative-improve
      (fn [old new] (< (Math/abs (- old new)) 0.00001))
      (fn [guess] (average guess (/ x guess))))
     initial-guess)))

(defn fixed-point [f initial-guess]
  ((iterative-improve
    (fn [old new] (< (Math/abs (- old new)) 0.00001))
    f) initial-guess))

(deftest test-sqrt-4
  (is #(< (Math/abs (- (sqrt 4) (Math/sqrt 4))) 0.00001)))

(deftest test-sqrt-10
  (is #(< (Math/abs (- (sqrt 10) (Math/sqrt 10))) 0.00001)))

(deftest test-fixed-point-cos
  (is (fn [_] (< (Math/abs (- (fixed-point #(Math/cos %) 1.0)
			      0.7390822))
		 0.00001))))

(deftest test-fixed-point-cos-plus-sin
  (is (fn [_] (< (Math/abs (- (fixed-point #(+ (Math/cos %) (Math/sin %)) 1.0)
			      1.258731))
		 0.00001))))



(comment
user> (fixed-point #(Math/cos %) 1.0)
;;=> 0.7390822985224024
user> (fixed-point #(+ (Math/cos %) (Math/sin %)) 1.0)
;;=> 1.2587315962971173
)

