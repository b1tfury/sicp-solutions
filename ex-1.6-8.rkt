#lang scheme

;; these methods will be used in below solutions

(define (abs x )
  (cond ((> x 0 ) x)
        ( (= x 0) 0 )
        ( (< x 0) ( - x))))


;; excercise 1.6
(define  (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; demo of new-if
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
(define (good-enough? guess x)
  (- (abs ( - guess x ) 0.001)))

(define (improve guess x)
  (average guess ( / x guess)))

(define (average x y)
  ( / ( + x y) 2))

(define (sqrt-iter guess x)
  ( new-if ( good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; demo for sqrt-new
(sqrt 2)


;; excercise 1.7
(define (good-enough-new? guess prev-guess)
  ( < (abs ( - guess prev-guess)) 0.001))

(define (sqrt-iter-new guess prev-guess  x)
  ( if ( good-enough-new? guess prev-guess)
       guess
       (sqrt-iter-new (improve guess x) guess x)))

(define (sqrt-new x)
  (sqrt-iter-new 1.0 x x))

;; New guess method
;; use difference between  prev guess and current guess
;; for evaluation


;; excercise 1.8
(define (good-enough-cube? guess prev-guess)
  ( < (abs ( - guess prev-guess)) 0.0001))

(define ( improve-cube x y )
  ( /  (+ (/ x ( * y y)) (* 2 y)) 3))

(define (cubert-iter guess prev-guess  x)
  ( if ( good-enough-cube? guess prev-guess)
       guess
       (cubert-iter (improve-cube guess x) guess x)))

(define (cubert x)
  (cubert-iter 1.0 x x))
