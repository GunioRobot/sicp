#lang racket

(define (stream-triplicates s)
  (if (= (stream-car s) (stream-car (stream-cdr s)) (stream-car (stream-cdr (stream-cdr s))))
      (cons-stream (stream-car s)
                   (stream-triplicates (stream-cdr (stream-cdr s))))
      (stream-triplicates (stream-cdr s))))

(define (square-sum i j)
  (+ (* i i) (* j j)))

(display-stream (stream-triplicates (stream-map (lambda (p)
                                                  (square-sum (car p) (car (cdr p))))
                                                (weighted-pairs integers integers square-sum))))

#|

325
425
650
725
845
850
925
1025
1105
1250
1300
1325
1445
1450
1525
1625
1690
1700
1825
1850
1885
2050
2125
2210
2225
2405
2425
2465
2525
2600
2650
2665

|#