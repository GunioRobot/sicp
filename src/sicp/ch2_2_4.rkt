#lang racket/load
;#lang planet neil/sicp
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
;(require (planet neil/sicp:1:13))

(define (identity x) x)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(paint (flipped-pairs einstein))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 2))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (below (beside painter bottom-right)
                 (beside top-left corner))))))

(define (square-limit painter n)
  (let ((corner (corner-split painter n)))
    (let ((half (beside (flip-horiz corner) corner)))
      (below (flip-vert half) half))))

;; ex 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split einstein 2))

;; higher order operations
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(paint (flipped-pairs2 einstein))

;; square-limit in terms of square-of-four
(define (square-limit2 painter n)
  (let ((square (square-of-four flip-horiz identity
                                rotate180  flip-vert)))
    (square (corner-split painter n))))

(paint (square-limit2 einstein 1))

;; ex 2.45
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))

(define usplit (split below beside))
(define rsplit (split beside below))

;; ex 2.46
;; A vector running from origin to (x,y) can be represented by a cons cell
(define (make-vect x y) (cons x y))
(define (xcor-vect v)   (car v))
(define (ycor-vect v)   (cdr v))

;; vector operations
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; ex 2.47
;; a
(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 frame)
  (car frame))

(define (edge1-frame1 frame)
  (car (cdr frame)))

(define (edge2-frame1 frame)
  (car (cdr (cdr frame))))

;; b
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (car (cdr frame)))

(define (edge2-frame2 frame)
  (cdr (cdr frame)))

;; for now use option a.
(define origin-frame origin-frame1)
(define edge1-frame  edge1-frame1)
(define edge2-frame  edge2-frame1)

;;; frames
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame))))))

;; ex 2.48
(define (make-segment v1 v2)    (cons v1 v2))
(define (start-segment segment) (car segment))
(define (end-segment segment)   (cdr segment))

;; ex 2.49
;; a. The painter that draws the outline of the designated frame.
(define one  0.99)
(define zero 0)

(define (outline frame)
  ((segments->painter (list (make-segment (make-vect zero zero) (make-vect one zero))
                            (make-segment (make-vect one zero) (make-vect one one))
                            (make-segment (make-vect one one) (make-vect zero one))
                            (make-segment (make-vect zero one) (make-vect zero zero))))
   frame))

;; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
(define (x-connect frame)
  ((segments->painter (list (make-segment (make-vect zero zero) (make-vect one one))
                            (make-segment (make-vect zero one) (make-vect one zero))))
   frame))
    
;; c.  The painter that draws a diamond shape by connecting the midpoints of the 
;;     sides of the frame.
(define (diamond frame)
  (let ((v1 (make-vect 0.5 0))
        (v2 (make-vect 0.999 0.5))
        (v3 (make-vect 0.5 0.999))
        (v4 (make-vect 0 0.5)))
    ((segments->painter (list (make-segment v1 v2)
                              (make-segment v2 v3)
                              (make-segment v3 v4)
                              (make-segment v4 v1))) 
     frame)))

;; d. The wave painter. 
(define (waveman frame)
  (let ((p1 (make-vect 0.25 0))
        (p2 (make-vect 0.4 0.4))
        (p3 (make-vect 0.25 0.5))
        (p4 (make-vect 0.15 0.4))
        (p5 (make-vect 0 0.7))
        (p6 (make-vect 0 0.8))
        (p7 (make-vect 0.15 0.55))
        (p8 (make-vect 0.25 0.6))
        (p9 (make-vect 0.4 0.6))
        (p10 (make-vect 0.35 0.75))
        (p11 (make-vect 0.4 0.9))
        (p12 (make-vect 0.6 0.9))
        (p13 (make-vect 0.65 0.75))
        (p14 (make-vect 0.6 0.6))
        (p15 (make-vect 0.8 0.6))
        (p16 (make-vect 0.999 0.25))
        (p17 (make-vect 0.999 0.1))
        (p18 (make-vect 0.6 0.4))
        (p19 (make-vect 0.75 0))
        (p20 (make-vect 0.65 0))
        (p21 (make-vect 0.5 0.2))
        (p22 (make-vect 0.35 0)))
    (let ((s1 (make-vect p1 p2))
          (s2 (make-vect p2 p3))
          (s3 (make-vect p3 p4))
          (s4 (make-vect p4 p5))
          (s5 (make-vect p6 p7))
          (s6 (make-vect p7 p8))
          (s7 (make-vect p8 p9))
          (s8 (make-vect p9 p10))
          (s9 (make-vect p10 p11))
          (s10 (make-vect p12 p13))
          (s11 (make-vect p13 p14))
          (s12 (make-vect p14 p15))
          (s13 (make-vect p15 p16))
          (s14 (make-vect p17 p18))
          (s15 (make-vect p18 p19))
          (s16 (make-vect p20 p21))
          (s17 (make-vect p21 p22)))
      ((segments->painter (list s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17))
       frame))))

         