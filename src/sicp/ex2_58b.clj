(ns sicp.ex2_58b
  (:refer-clojure :exclude (number?))
  (:use [clojure.test]
        [sicp.utils]))

;;; differentiation of infix expressions
;;    part b. Assume standard algebraic form.
;;
(defn third [x]
  (if (= (count x) 3)
    (second (rest x))
    (rest (rest x))))

(defn same-op? [op x]
  (= op x))

(defn peek-op [expr]
  (second expr))

(defn- first-expr* [expr op]
  (cond (and (nil? (peek-op expr)) (empty? expr)) nil
        (and (same-op? op (peek-op expr))
             (= op '*)) (cons (first expr) (cons (second expr) (first-expr* (rest (rest expr)) op)))
        :else (list (first expr))))

(defn first-expr [expr]
  (let [op (second expr)]
    (when (not (nil? op))
      (first-expr* expr op))))

(defn- rest-expr* [expr op]
  (cond (empty? expr) nil
        (and (same-op? op (peek-op expr)) (= op '*)) (rest-expr* (rest (rest expr)) op)
        :else (rest (rest expr))))

(defn rest-expr [expr]
  (let [op (second expr)]
    (when (not (nil? op))
      (rest-expr* expr op))))

(defn- op-expr* [expr op]
  (cond (empty? expr) nil
        (same-op? op (peek-op expr)) (op-expr* (rest (rest expr)) op)
        :else (if (= op '*) (peek-op expr) op)))

(defn op-expr [expr]
  (let [op (second expr)]
    (when (not (nil? op))
      (op-expr* expr op))))

(defn exponentiation? [exp]
  (= (second exp) '**))

(defn base [exp]
  (first exp))

(defn exponent [exp]
  (third exp))

(defn variable? [x]
  (if (and (list? x)
           (= (count x) 1))
    (symbol? (first x))
    (symbol? x)))

(defn same-variable? [v1 v2]
  (cond (list? v1) (and (variable? v1)
                        (variable? v2)
                        (= (first v1) v2))
        (list? v2) (and (variable? v1)
                        (variable? v2)
                        (= v1 (first v2)))        
        :else (and (variable? v1)
                   (variable? v2)
                   (= v1 v2))))

(defn number? [exp]
  (if (and (list? exp)
           (= (count exp) 1))
    (clojure.core/number? (first exp))
    (clojure.core/number? exp)))

(defn =number? [exp num]
  (and (number? exp)
       (= exp num)))

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
  (and (list? x) (= (op-expr x) '+)))

(defn addend [s]
  (first-expr s))

(defn augend [s]
  (rest-expr s))

(defn product? [x]
  (= (second x) '*))

(defn multiplier [p]
  (first p))

(defn multiplicand [p]
  (rest (rest p)))

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
        :else (str "unknown expression type -- deriv " exp)))


(deftest test-deriv-and-helpers
  (let [e1 1
        e2 '(x)
        e3 '(x + 1)
        e4 '(x * y)
        e5 '(x * x)
        e6 'x
        e7 '(x + 2 * x + 2)
        e8 '(x * y + 1)]
    (are [p q] [= p q]
         (first-expr e3) '(x)
         (op-expr e3)    '+
         (rest-expr e3)  '(1)
         (first-expr e4) '(x * y)
         (op-expr e4)    nil
         (rest-expr e4)  ()
         (first-expr e7) '(x)
         (op-expr e7) '+
         (rest-expr e7) '(2 * x + 2)
         (first-expr e8) '(x * y)
         (op-expr e8) '+
         (rest-expr e8) '(1))))

















