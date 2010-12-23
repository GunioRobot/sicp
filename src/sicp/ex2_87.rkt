#lang racket

(put '=zero? '(polynomial)
     (lambda (p)
       (define (terms-=zero? terms)
         (if (empty-termlist? terms)
             #t
             (let ((term (first-term terms)))
               (let ((c (coeff term)))
                 (let ((type (type-tag c)))
                   (let ((zproc (get '=zero? (list type))))
                     (if (zproc c)
                         (terms-=zero? (rest-terms terms))
                         #f)))))))
       
       (terms-=zero? (term-list p))))