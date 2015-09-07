#lang scheme

;; Solution 1.40
(define (square a) (* a a))
(define (cubic a b c)
  (lambda (x)
    (+ (* x (square x)) (* a (square x)) (* b x) c)))


;; Solution 1.41
(define (inc a) (+ a 1))
(define (double f)
  ( lambda(x) (f (f x))))

(((double (double double )) inc) 5)
;; Value = 21

;; Solutin 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)
;; value =  49


;; Solution 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)
;; value = 625

;; Solution 1.44
(define (average-new a b c)
  (/ (+ a b c) 3))

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (average-new (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smoothed n )
    (lambda (f x) (((repeated smooth n) f )x)))

((n-fold-smoothed 10) square 2)


;;  Solution 1.45
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nthroot x n)
  (define (log2 x)
    (/ (log x) (log 2)))
  (let ((mtimes (floor (log2 n))))
    (fixed-point-of-transform
     (lambda (y) (/ x (expt y (- n 1))))
     (repeated average-damp mtimes)
     1.0)))

(nthroot 1024 20)


;; Solution 1.46

(define (iterative-improve goog-enough? improve-guess)
  (lambda (guess)
    (if (goog-enough? guess)
        guess
        ((iterative-improve goog-enough? improve-guess)
         (improve-guess guess)))))

(define (sqrt-iter x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve-guess guess)
    (define (average a b)
      (/ (+ a b) 2))
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))
;
(sqrt-iter 3.0)
;Value: 1.7320508100147274

(define (fixed-point-new f guess)
  (define (good-enough? guess)
    (let ((next-guess (f guess)))
      (< (abs (- next-guess guess)) 0.0001)))
  (define improve-guess f)
  ((iterative-improve good-enough? improve-guess) guess))
;
(fixed-point-new cos 1.0)
