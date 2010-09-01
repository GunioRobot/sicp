(ns sicp.ex2_44
  (:use [sicp.ch2-2 :only (beside below)]))

(defn up-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (- n 1))]
      (below painter (beside smaller smaller)))))
