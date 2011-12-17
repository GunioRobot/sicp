#lang racket

(require rackunit
         "ch2_3.rkt")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (cond [(empty? (rest set)) (first set)]
        [else (successive-merge (adjoin-set (make-code-tree (first set)
                                                            (second set))
                                            (rest (rest set))))]))

(test-begin
 (let ([sample-tree (make-code-tree (make-leaf 'A 4)
                                    (make-code-tree
                                     (make-leaf 'B 2)
                                     (make-code-tree (make-leaf 'D 1)
                                                     (make-leaf 'C 1))))])
   (check equal?
          (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
          sample-tree)))

(provide generate-huffman-tree)