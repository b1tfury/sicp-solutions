#lang scheme

;; Linear Recursion and Iteration

;; recursive definition of factorial

(define (factorial  n)
  ( if ( = n 1)
       1
       (* n (factorial (- n 1)))))

;; iterative defintion of factorial

(define (fact-iter ans counter n)
  ( if ( > counter n)
       ans
       (fact-iter (* ans counter) (+ counter 1) n)))
(define (factorial-new n)
  (fact-iter 1 1 n))
