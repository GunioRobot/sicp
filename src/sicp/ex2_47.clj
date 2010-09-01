(ns sicp.ex2_47
  (:use [clojure.test]))

;; first approach
(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

;; second approach
(defn make-frame [origin]
  (cons origin (cons edge1 (list edge2))))

;; for both of them the following selectors will work
(defn origin-frame [frame]
  (first frame))

(defn edge1-frame [frame]
  (second frame))

(defn edge2-frame [frame]
  (first (rest (rest frame))))

