(ns sicp.ex2_55
  (:use [clojure.test]))

(deftest test-double-quote
  (is [= (first ''abracadabra)
         'quote]))

(comment
  "(first ''abcde) expands to (first (quote (quote (abcde)))
  which is (quote (abcde)), first of which is quote. The outer
  quote makes the inner quote a symbol and does not evaluate it."
  )