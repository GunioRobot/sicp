#lang racket

#|

promitive procedures are applied as follows:

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

Let us say that we have added `map' as a primitive procedure. It is represented as
('primitive map)

(primitive-implementation)

> (define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'map map)
        ))
> (define (primitive-procedure-names)
  (map car
       primitive-procedures))
> (primitive-procedure-names)
'(car cdr cons null? map)
> (define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
> (primitive-procedure-objects)
'((primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:map>))

> (apply (primitive-implementation (list 'map map)) (primitive-implementation (list 'car car)) '(((1 2 3) (3 4 5))))
'(1 3)
> (apply (primitive-implementation (list 'primitive map)) (primitive-implementation (list 'primitive car)) '(((1 2 3) (3 4 5))))
'(1 3)
> (apply (primitive-implementation (list 'primitive map)) (list 'primitive car) '(((1 2 3) (3 4 5))))
map: expects type <procedure> as 1st argument, given: '(primitive #<procedure:car>); other arguments were: '((1 2 3) (3 4 5))
> 

If we apply primitive procedures to our primitive map, it fails because it cannot understand our primitive representation.
In general, all primitive versions of higher order functions fail when passes a primitive procedure.

If we are implementing Scheme using a Lisp other than Scheme itself (say, Clojure) then we will be passing Scheme functions
to the Clojure's map, which is not guaranteed to work because of the different syntax.
|#