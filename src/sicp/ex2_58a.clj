(ns sicp.ex2_58a
  (:use [clojure.test]
        [sicp.utils]))

;;; differentiation of infix expressions
;;    part a. Assume proper brackets around compound expressions.
;;
(defn third [x]
  (when (list? x)
    (second (rest x))))

(defn exponentiation? [exp]
  (= (second exp) '**))

(defn base [exp]
  (first exp))

(defn exponent [exp]
  (third exp))

(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list a1 '+ a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list m1 '* m2)))

(defn sum? [x]
  (and (list? x) (= (second x) '+)))

(defn addend [s]
  (first s))

(defn augend [s]
  (third s))

(defn product? [x]
  (and (list? x) (= (second x) '*)))

(defn multiplier [p]
  (first p))

(defn multiplicand [p]
  (third p))

(defn make-exponentiation [b n]
  (cond (=number? b 1) 1
        (=number? b 0) 0        
        (=number? n 1) b
        (=number? n 0) 1
        (and (number? b) (number? n)) (Math/pow b n)
        :else (list b '** n)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) var))
                                 (make-product (deriv (multiplier exp) var)
                                               (multiplicand exp)))
        (exponentiation? exp) (make-product (exponent exp)
                                            (make-product (make-exponentiation (base exp)
                                                                               (- (exponent exp) 1))
                                                          (deriv (base exp) var)))
        :else (str "unknown expression type -- derive " exp)))
