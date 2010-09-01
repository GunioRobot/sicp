(ns sicp.ex2_51
  (:use [clojure.test]
        [sicp.ch2_2 :only (transform-painter)]))

(defn below1 [painter1 painter2]
  (let [paint-low (transform-painter painter1
                                     (make-vect 0 0)
                                     (make-vect 1 0)
                                     (make-vect 0 0.5))
        paint-high (transform-painter painter2
                                      (make-vect 0 0.5)
                                      (make-vect 1 0)
                                      (make-vect 0 1))]
    (fn [frame]
      (paint-low frame)
      (paint-high frame))))

(defn below2 [painter1 painter2]
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))