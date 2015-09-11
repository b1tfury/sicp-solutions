#lang scheme

(define dx 0.005)

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx))))
