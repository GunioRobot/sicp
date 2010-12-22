(define (foobar x)
  (cond
   ((= x 1) 1)
   ((= x 2) 1)
   (else (+ (foobar (- x 1))
            (foobar (- x 2))))))