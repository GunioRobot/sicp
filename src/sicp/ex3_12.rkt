#lang r5rs

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

#|

x => a -> b -> ()
y => c -> d -> ()

(cdr x) => (b->()) => (b)

w => (a->b->c->d->()) and
x => (a->b->c->d->())

(cdr x) => b->c->d->() (b c d)
   
|#