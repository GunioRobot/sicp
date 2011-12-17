(ns sicp.ex2_56
  (:use [sicp.ch2_3 :exclude (deriv)]
        [sicp.utils]
        [clojure.test]))

(declare exponentiation? exponent base make-exponentiation)

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
        :else (error "unknown expression type -- derive")))

(defn exponentiation? [exp]
  (= (first exp) '**))

(defn base [exp]
  (second exp))

(defn exponent [exp]
  (second (rest exp)))

(defn make-exponentiation [b n]
  (cond (=number? b 1) 1
        (=number? b 0) 0
        (=number? n 1) b
        (=number? n 0) 1
        (and (number? b) (number? n)) (Math/pow b n)
        :else (list '** b n)))