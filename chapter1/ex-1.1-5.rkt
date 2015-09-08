#lang scheme


;; Exercise 1.1 to 1.5


;; Solution 1.1

10
(+ 5 3 4)
(- 9 1 )
(/ 6 2)
( + ( * 2 4 ) ( - 4 6 ))
(define a 3)
(define b ( + a 1))
(+ a b (* a b))
(= a b)
(if ( and ( > b a) ( < b ( * a b)))
    b
    a)
(cond ( ( = a 4) 6 )
      (( = b 4) ( + 6 7 a))
      (else 25 ))
(+ 2 ( if  (> b a )
           b
           a )
   )
( * (cond (( > a b) a )
          (( < a b ) b)
          (else -1))
    ( + a 1))

;; Solution 1.2

(/ ( + 5 4 ( - 2 ( - 3 ( + 6 ( / 4 5 ))))) ( * 3 ( - 6 2) (- 2 7)))


;; Solution 1.3

(define (square x) ( * x x))

(define (sum-three x y z)
  (if (and (< x y) (< x z)) ( + (square y) (square z))
      (if (and (< y x) (< y z)) ( + (square x) (square z))
          ( + (square y) (square z) ))))


;; Solution 1.4

(define (a-plus-abs-b a b)
  ((if ( > b 0)
       +
       - )
   a b))


;; Solution 1.5

(define (p) (p))

(define (test x y)
  (if ( = x 0)
      0
      y))

;; (test 0 (p))
;; => stuck in infinite loop

;; When expression (test 0 (p)) is evaluated;
;; then the argument are evaluated before the function call.
;; This expression evaluates to an infinite loop.
