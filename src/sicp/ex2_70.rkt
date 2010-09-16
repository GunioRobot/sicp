#lang racket
(require rackunit
         "ex2_69.rkt"
         "ex2_68.rkt")

(define symbol-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define huffman-tree (generate-huffman-tree symbol-pairs))

(define test-message '(GET A JOB
                       SHA NA NA NA NA NA NA NA NA
                       GET A JOB
                       SHA NA NA NA NA NA NA NA NA
                       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                       SHA BOOM))

(check = 
       (length (encode test-message 
                       huffman-tree))
       84)

#|
a. Encoding requires 84 bits.
b. There are 8 symbols, so we use 3 bits/symbol.
   (length test-message) is 36. So total length of the encoded
   song, had it been fixed length code, will be (* 36 3) = 108
|#

