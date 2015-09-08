#lang scheme

;; Solution 1.34
(define (f g)
  (g 2))
(define (square x) (* x x))

(f square)
;; value 4
(f (lambda (z) (* z (+  z 1))))
;; value 6
(f f)
;; not procedure error
