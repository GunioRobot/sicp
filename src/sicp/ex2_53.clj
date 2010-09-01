(ns sicp.ex2_53
  (:use [clojure.test]
        [sicp.ch2_3 :only (memq)]))

(deftest test-quoted-expr
  (are [x y] [= x y]
       (list 'a 'b 'c)           '(a b c)
       (list (list 'george))     '((george))
       (rest '((x1 x2) (y1 y2))) '((y1 y2))
       (second '((x1 x2) (y1 y2))) '(y1 y2)
       (list? (first '(a short list))) false
       (memq 'red '((red shoes) (blue socks))) false
       (memq 'red '(red shoes blue socks)) '(red shoes blue socks)))