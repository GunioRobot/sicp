#lang scheme

(require (planet soegaard/sicp:2:1/sicp))

;; count-leaves
(define (count-leaves x)
  (cond
    [(null? x) 0]
    [(not (pair? x)) 1]
    [else (+ (count-leaves (first x))
             (count-leaves (rest x)))]))

;; scale tree
(define (scale-tree tree factor)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (* tree factor)]
        [else (cons (scale-tree (first tree) factor)
                    (scale-tree (rest tree) factor))]))

(define (scale-tree-map tree factor)
  (map (lambda (x)
         (if (not (pair? x))
             (* x factor)
             (scale-tree-map x factor)))
       tree))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (identity x) x)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
