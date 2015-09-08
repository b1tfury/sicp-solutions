#lang scheme

;; Solutiob 1.35
(define (close-enough? v1 v2 tolerance)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next phi-tolerance)
          next
          (try next))))
  (try first-guess))

(define phi-tolerance 0.00001)
(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))



;; Solution 1.36

(define (fixed-point-new f first-guess)
  (define (try guess)
    (let ((next (f guess)))
          (display next)
          (newline)
      (if (close-enough? guess next phi-tolerance)
          next
          (try next ))))
  (try first-guess))

; without average damping
(fixed-point (lambda (x) (/ (log 1000.) (log x)))
             10.0)

; with average damping
(define average (lambda (a b) (/ (+ a b) 2)))
(fixed-point (lambda (x) (average x (/ (log 1000.) (log x))))
             10.0)


;; Solution 1.37

; iterative
(define (cont-frac-i n d k)
  (define (iter counter result)
    (if (< counter 1)
        result
        (iter (- counter 1) (/ (n counter) (+ (d counter) result)))))
  (iter k 0))
(cont-frac-i (lambda (i) 1.0)
             (lambda (i) 1.0)
             100)

; recursive
(define (cont-frac-r n d k)
  (define (recu ibeg iend)
    (if (= ibeg iend)
        (/ (n ibeg) (d ibeg))
        (/ (n ibeg) (+ (d ibeg) (recu (+ ibeg 1) iend)))))
  (recu 1 k))
(cont-frac-r (lambda (i) 1.0)
             (lambda (i) 1.0)
             100)

; How large must k be ...?
(define (find-k small)
  (define phi-1 (/ 2 (+ (sqrt 5) 1)))
  (define (try k)
    (let ((result (cont-frac-i (lambda (i) 1.0)
                               (lambda (i) 1.0)
                               k)))
      (if (< (abs (- result phi-1)) small)
          k
          (try (+ k 1)))))
  (try 2))
(find-k 0.0001)
;Value: 10

;; Solution 1.38

(define (e k)
  (define n (lambda (i) 1.0))
  (define d (lambda (i)
              (let ((r (remainder i 3))
                    (f (floor (/ i 3))))
                (if (= r 2)
                    (* 2 (+ f 1))
                    1.0))))
  (+ 2 (cont-frac-i n d k)))

(e 100)


;; Solution 1.39
(define (square x) (* x x))
(define (tan-cf x k)
  (define n (lambda (i)
              (if (= i 1)
                  x
                  (- (square x)))))
  (define d (lambda (i)
              (- (* 2 i) 1)))
  (cont-frac-i n d k))

(tan-cf (/ 3.1415926 4) 10)
;;value 0.9999999732051038
