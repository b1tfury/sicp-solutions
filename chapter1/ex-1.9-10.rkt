#lang scheme

;; Solution 1.9

(define (inc a) (+ a 1))

(define (dec a) (- a 1))

(define (plus-recursive a b)
  (if (= a 0)
      b
      (inc (plus-recursive (dec a)   b))))

(define (plus-iter a b)
  (if (= a 0)
      b
      (plus-iter (dec a) (inc b))))

;; These both methods are implemetation but iter version is faster then recursive version


;; Solution 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))
