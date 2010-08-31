(ns pictlang.core
  (:import [javax.swing JPanel JFrame]
           [java.awt Color Graphics Graphics2D]
           [java.awt.image BufferedImage]))

(def dim-frame [256 256])

(def img   (BufferedImage. (dim-frame 0) (dim-frame 1) (BufferedImage/TYPE_INT_RGB)))
(def bg    (.getGraphics img))

(defn draw-line [v1 v2]
  (.setPaint bg Color/RED)
  (.drawLine bg
             (- (dim-frame 0) (* (first v1) (dim-frame 0)))
             (- (dim-frame 1) (* (second v1) (dim-frame 1)))
             (- (dim-frame 0) (* (first v2) (dim-frame 0)))
             (- (dim-frame 1) (* (second v2) (dim-frame 1)))))

(defn start-picture []
  (let [frame (JFrame.)
        panel (doto (proxy [JPanel] []
                      (paint [g]
                             (.drawImage g img 0 0 this))))]
    (doto bg
      (.setColor Color/BLACK)      
      (.fillRect 0 0 (dim-frame 0) (dim-frame 1)))
    
    (doto frame
      (.add panel)
      (.setSize (dim-frame 0) (dim-frame 1))
      (.show)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))))

(defn show [picture frame]
  (picture frame))

