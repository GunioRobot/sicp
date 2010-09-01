(ns sicp.ex2_48
  (:use [clojure.test]
        [sicp.ex2_46]
        [sicp.ch2_2 :only (segments->painter)]))

;; we define a segment as a list of 2 vectors
(defn make-segment [v1 v2]
  (list v1 v2))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))