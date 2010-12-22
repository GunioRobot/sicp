#lang racket

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let* ((e (entry set-of-records))
             (k (key e)))
        (cond ((equal? given-key k) e)
              ((< k given-key) (lookup given-key (left-branch set-of-records)))
              ((> k given-key) (lookup given-key (right-branch set-of-records)))))))
        
