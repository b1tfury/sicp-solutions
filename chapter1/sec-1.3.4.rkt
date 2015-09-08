#lang scheme

;; Procedures as Returened values
(define (average a b) ( / ( + a b) 2))

(define (square x) (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
((deriv square) 4)
