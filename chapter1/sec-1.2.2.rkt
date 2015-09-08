#lang scheme

;;Tree Recursion

;;recursive defintion to calculate  fibonanci number

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; iterative definition to calculate fibonacci number

(define (fib-iter curr prev n)
  (if (not (>  n 1))
      curr
      (fib-iter (+ curr prev) curr (- n 1))))
(define (fib-new n)
  (fib-iter 1 0 n))

;; again the iterative version is much faster than recursive number
