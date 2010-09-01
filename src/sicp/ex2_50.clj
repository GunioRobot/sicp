(ns sicp.ex2_50
  (:use [clojure.test]
        [sicp.ch2_2 :only (rotate90)]))

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(defn rotate180 [painter]
  ((repeatedly 2 rotate90) painter))

(defn rotate270 [painter]
  ((repeatedly 3 rotate90) painter))