(ns sicp.ex2_49
  (:use [sicp [ch2_2 :only (segments->painter)]]
        [clojure.test]))

;;; Use segments->painter to define the following primitive painters:

;;   a.  The painter that draws the outline of the designated frame.

(def o  (make-vect 0 0))
(def lr (make-vect 0 1))
(def ul (make-vect 1 0))
(def ur (make-vect 1 1))

(defn frame-outline [frame]
  (let [s1 (make-segment o lr)
        s2 (make-segment lr ur)
        s3 (make-segment ur ul)
        s4 (make-segment ul o)]
    ((segments->painter (list s1 s2 s3 s4)) frame)))

;; b The painter that draws an ``X'' by connecting opposite corners
;;   of the frame.
(defn cross-segments [frame]
  (let [s1 (make-segment ul lr)
        s2 (make-segment o ur)]
    ((segments->painter (list s1 s2)) frame)))

;; c.  The painter that draws a diamond shape by connecting the midpoints
;;     of the sides of the frame.
(defn connect [vs]
  (let [new-list   (append vs (list (car vs)))
        vect-pairs (partition 2 1 new-list)
        segments   (map #(apply make-segment %) vect-pairs)]
    segments))

(defn diamond-segments [frame]
  (let [m1 (make-vect 0 0.5)
        m2 (make-vect 0.5 1)
        m3 (make-vect 1 0.5)
        m4 (make-vect 0.5 0)]
    ((segments->painter (connect (list m1 m2 m3 m4))) frame)))

;; d. wave painter
(defn wave [frame]
  (let [p01 (make-vect 0.40 1.00)
        p02 (make-vect 0.60 1.00)
        p03 (make-vect 0.00 0.80)
        p04 (make-vect 0.35 0.80)
        p05 (make-vect 0.65 0.80)
        p06 (make-vect 0.00 0.60)
        p07 (make-vect 0.30 0.60)
        p08 (make-vect 0.40 0.60)
        p09 (make-vect 0.60 0.60)
        p10 (make-vect 0.70 0.60)
        p11 (make-vect 0.20 0.55)
        p12 (make-vect 0.30 0.55)
        p13 (make-vect 0.35 0.50)
        p14 (make-vect 0.65 0.50)
        p15 (make-vect 0.20 0.45)
        p16 (make-vect 1.00 0.40)
        p17 (make-vect 0.50 0.20)
        p18 (make-vect 1.00 0.20)
        p19 (make-vect 0.25 0.00)
        p20 (make-vect 0.40 0.00)
        p21 (make-vect 0.60 0.00)
        p22 (make-vect 0.75 0.00)]
    ((segments->painter
      (list (make-segment p01 p04)
            (make-segment p04 p08)
            (make-segment p08 p07)
            (make-segment p07 p11)
            (make-segment p11 p03)
            (make-segment p06 p15)
            (make-segment p15 p12)
            (make-segment p12 p13)
            (make-segment p13 p19)
            (make-segment p20 p17)
            (make-segment p17 p21)
            (make-segment p22 p14)
            (make-segment p14 p18)
            (make-segment p16 p10)
            (make-segment p10 p09)
            (make-segment p09 p05)
            (make-segment p05 p02))) frame)))
