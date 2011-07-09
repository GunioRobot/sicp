#lang racket

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

#|

> (display-stream S)

1
2
3
4
5
6
8
9
10
12
15
16
18
20
24
25
27
30
32
36
40
45
48
50
54
60
64
72

|#
