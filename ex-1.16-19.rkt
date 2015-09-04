#lang scheme

;; Solution 1.16
;; iterative implementaion of exponenetial
(define (abs x)
  (if ( < x 0)
      (* x -1)
      (x)))

(define (square x) (* x x))

(define (even? a)
  (= (remainder a 2) 0))

(define (expt-iter ans b a)
  (cond ((= a 0) ans)
        ((even? a) (expt-iter ans (square b ) (/ a 2)))
        (else (expt-iter (* ans b) b (- a 1)))))

(define (expt b a)
  (expt-iter 1 b a))


;; Solution 1.17/ 1.18

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (* a b)
  (if (or (= b 0) (= a 0))
      0
      (if (even? b)
          (double (* a (halve b)))
          (+ a ( * a (- b 1))))))

;; the above solution will take time of order O(log(n))


;; Solution 1.19
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b (+ (square p) (square q)) (+ (* p q) (square q)) (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(define (fib n)
  (fib-iter 1 0 0 1 n))
