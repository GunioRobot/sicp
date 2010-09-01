(ns sicp.ex2_55
  (:use [clojure.test]))

(deftest test-double-quote
  (is [= (first ''abracadabra)
         'quote]))