(ns sicp.ch2_3)

(defn memq [item x]
  (cond
    (empty? x) false
    (= (first x) item) x
    :else (memq item (rest x))))