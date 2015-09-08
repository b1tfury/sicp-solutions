#lang scheme

;; Introduction to data abstraction

;; Arithmatic operations for rational numbers

;; wishful thinking
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error "Arguments not 0  or 1 : Cons" m))))
  dispatch)
(define (car z) (z 0))

(define (cdr z) (z 1))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (make-rat n d)
  (let ((g (gcd n d)))
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

;; 2.1.3 What is meant  by data


(define a (cons 1 2))

(car a)

(cdr a)
