#lang scheme

;; Solution 2.1

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g)
          (/ d g))))
(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  ( make-rat (- (* (numer x) (denom y))
                (* (numer y) (denom x)))
             (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(define one-half (make-rat 1 2))

(print-rat one-half)


;; 2.1.2  Abstraction barriers
(define (average x y) ( / (+ x y) 2))

(define (point x y) (cons x y))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (make-segment start end)
  (cons start end))

(define (midpoint-segment line)
  (cons (average (car (start-segment line)) (car (end-segment line)))
        (average (cdr (start-segment line)) (cdr (end-segment line)))))
