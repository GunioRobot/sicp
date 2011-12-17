#lang r5rs

#|

mystery is trying to find the reverse of the list x. Too lazy to draw the ascii art.
Too lazy to scan my notebook.

|#

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))