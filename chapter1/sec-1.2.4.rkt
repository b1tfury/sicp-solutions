#lang scheme

;; Exponentiation

;; linear recursive procedure of getting exponential
(define (expt b a)
  (if (= a 0)
      1
      ( * b (expt b (- a 1)))))

;; linear iterative approach of getting exponential
(define (expt-iter ans b counter)
  (if (= counter 0)
      ans
      (expt-iter (* ans b) b (- counter 1))))

(define (expt-new b a)
  (expt-iter 1 b a))

;; faster implementation of exponential
(define (square x) (* x x))

(define (even? a)
  (=  (remainder a 2) 0))

(define (fast-expt b a)
  (cond ((= a 0) 1)
        ((even? a) (square (fast-expt b (/ a 2))))
        (else (* b (fast-expt b (- a 1))))))

;; last approach has complexity od (log(n))
