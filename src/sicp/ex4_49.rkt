#lang racket

#|

Process 1 needs access to a shared resource depending on some runtime condition and needs
another resource depending on another condition.

Consider another similar process being executed concurrently. We cannot determine
the order of the resource acquisition apriori in this case.

|#