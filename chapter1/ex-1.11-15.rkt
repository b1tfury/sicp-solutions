#lang scheme

;; Solution 1.11
;; recursive procedure
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;;iterative procedure
(define (f-iter a b c n)
  (if (< n 3)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
(define (f-new n)
  (f-iter 2 1 0 n))


;; Solution 1.12

(define (pascal row col)
  (cond ((= row 1) 1)
        ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

;; Solution 1.15

(define (cube x) ( * x x x ))

(define (p x)
  (print "1\n")
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
