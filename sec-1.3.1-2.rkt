#lang scheme

;; Procedurea as Arguments

(define (cube a) (* a a a))

(define (sum-integers a b)
  (if (> a b)
      0
      ( + a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b) 0
      ( + (term a)
          (sum term (next a) next b))))
(define (inc a ) (+ a 1))

(define (sum-cubes-new a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-ints a b)
  (sum identity a inc b))
(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))
(define (pi-next x)
  (+ x 4))

(define (pi-sum-new a b)
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


;; Consturcting procedures using lambdas

(define plus4 (lambda (x) (+ x 4)))

(define (square a) (* a a))
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
