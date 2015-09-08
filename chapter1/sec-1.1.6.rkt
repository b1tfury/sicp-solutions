#lang scheme

;; Conditional Expression and Predicates

(define (abs x )
  (cond ((> x 0 ) x)
        ( (= x 0) 0 )
        ( (< x 0) ( - x))))

(define (abs-using-one-cond x)
  (cond ( (< x 0 ) (- x))
        (else x )))

(define (abs-using-if x)
  ( if (< x 0)
       ( - x )
       x ))

(define (ex-or-predicate x y)
  (if (or  ( < x 0 ) ( > y 0))
      x
      y))
