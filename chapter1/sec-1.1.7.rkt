#lang scheme

;; these methods will be used in belows problems
(define (abs x )
  (cond ((> x 0 ) x)
        ( (= x 0) 0 )
        ( (< x 0) ( - x))))


;; Example: Square Roots by Newton's Method

(define (square x)
  (* x x))

(define (good-enough? guess x)
  ( < (abs (- (square guess) x) ) 0.001))

(define (improve guess x)
  (average guess ( / x guess)))

(define (average x y)
  ( / ( + x y) 2))

(define (sqrt-iter guess x)
  ( if ( good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))1
