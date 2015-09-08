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


;;  Solution 2.2
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


;; Solution  2.3
(define (rectangle l b)
  (cons l b))

(define (len rect)
  (car rect))

(define (bred rect)
  (cdr rect))

(define (perimeter rect)
   (* 2 (+ (len rect) (bred rect))))

(define (area rect)
  (* (len rect) (bred rect)))

(define rect (rectangle 10 5))

(len rect)

(bred rect)

(perimeter rect)

(area rect)


;; Solution 2.4
(define (cons-new x y)
  (lambda (m) (m x y)))

(define (car-new z)
  (z (lambda (p q) p)))

(define (cdr-new z)
  (z (lambda (p q) q)))

(define z (cons-new 1 2))

(car-new z)

(cdr-new z)

;; Solution 2.5
(newline)
(display "Solution 2.5")
(newline)
(define (pow a b)
  (if (= b 0)
      1
      (* a (pow a (- b 1)))))

(define (cons-ab x y)
  (define (dispatch m)
    (cond ((= m 0) (pow 2 x))
          ((= m 1) (pow 3 y))
          (else
           (error "Argument is neither 0 or 1. Cons: " m))))
  dispatch)

(define (car-ab z) (z 0))

(define (cdr-ab z) (z 1))

(define new-z (cons-ab 2 3))

(car-ab new-z)

(cdr-ab new-z)


;; Solution 2.6
(newline)
(display "Solution 2.6")
(newline)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f ) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(= (add-1 zero) one)
