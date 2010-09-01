(ns sicp.ex2_45)

(defn split [f1 f2]
  (fn (painter n)
    (if (= n 0)
      painter
      (let [smaller ((split f1 f2) painter (- n 1))]
        (f1 painter (f2 smaller smaller))))))

(def right-split (split beside below))
(def up-split    (split below beside))

    