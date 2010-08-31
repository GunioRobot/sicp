(ns pictlang.utils
  (:use [sicp.ex2_23 :only (for-each)]
        [pictlang.core :only (draw-line)]))

(declare flip-vert up-split flip-horiz transform-painter rotate90 rotate270)

(defn make-vect [x y]
  (list x y))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (second v))

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(defn scale-vect [v s]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(defn origin-frame [frame]
  (first frame))

(defn edge1-frame [frame]
  (second frame))

(defn edge2-frame [frame]
  (first (rest (rest frame))))

;; we define a segment as a list of 2 vectors
(defn make-segment [v1 v2]
  (list v1 v2))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))


(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0)
        paint-left (transform-painter painter1
                                      (make-vect 0 0)
                                      split-point
                                      (make-vect 0 1))
        paint-right (transform-painter painter2
                                       split-point
                                       (make-vect 1 0)
                                       (make-vect 0.5 1))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(defn below [painter1 painter2]
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(defn flipped-pairs [painter]
  (let [painter2 (beside painter (flip-vert painter))]
    (below painter2 painter2)))

(defn right-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (right-split painter (- n 1))]
      (beside painter (below smaller smaller)))))

(defn corner-split [painter n]
  (if (= n 0)
    painter
    (let [up       (up-split painter (- n 1))
          right    (right-split painter (- n 1))
          top-left (beside up up)
          bottom-right (below right right)
          corner   (corner-split painter (- n 1))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half    (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (edge1-frame frame) (xcor-vect v))
               (scale-vect (edge2-frame frame) (ycor-vect v))))))

(defn segments->painter [segment-list]
  (fn [frame]
    (for-each
     (fn [segment]
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


;; transformation
(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame new-origin
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(defn shrink-to-upper-right [painter]
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(defn squash-inwards [painter]
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(defn up-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (- n 1))]
      (below painter (beside smaller smaller)))))

(defn split [f1 f2]
  (letfn [(split-fn [painter n]
                    (if (= n 0)
                      painter
                      (let [smaller (split-fn painter (- n 1))]
                        (f1 painter (f2 smaller smaller)))))]
    split-fn))

(def right-split (split beside below))
(def up-split    (split below beside))

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(defn rotate180 [painter]
  ((repeatedly 2 rotate90) painter))

(defn rotate270 [painter]
  ((repeatedly 3 rotate90) painter))
