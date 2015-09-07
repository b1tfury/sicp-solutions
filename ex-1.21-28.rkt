#lang scheme

;;Solution 1.21
(define (square a) (* a a))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(smallest-divisor 199) ;; 199
(smallest-divisor 1999) ;; 1999
(smallest-divisor 19999) ;; 7


;;Soltion 1.22
(newline)
(newline)
(display "Solution 1.24")
(newline)
(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  true)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      false))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes start end)
   (if (even? start)
       (search-for-primes (+ start 1) end)
       (cond ((< start end) (timed-prime-test start)
                            (search-for-primes (+ start 2) end)))))

(define (even? n)
  (= (remainder n 2) 0))

(search-for-primes 1000 1020)
(search-for-primes 10000 10039)
(search-for-primes 100000 100044)
(search-for-primes 1000000 1000038)
;; The increase in time is in order of log(n)


;; Solution 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (timed-prime-test-new n)
  (start-prime-test-new n (current-inexact-milliseconds)))

(define (start-prime-test-new n start-time)
  (if (fast-prime? n 2)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      false))

(define (search-for-primes-new start end)
   (if (even? start)
       (search-for-primes-new (+ start 1) end)
       (cond ((< start end) (timed-prime-test-new start)
                            (search-for-primes-new (+ start 2) end)))))

(newline)
(newline)
(display "Solution 1.24")
(newline)

(search-for-primes-new 1000 1020)
(search-for-primes-new 10000 10039)
(search-for-primes-new 100000 100044)
(search-for-primes-new 1000000 1000038)


;;Solution 1.26
;; The time taken here is in O(n), because mulutplication operator is called after recursive application of expmod


;; Solution 1.27
(define (carmichael? n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base (expmod base (- exp 1) m))
                      m))))
  (define (fermat-test n a)
    (= (expmod a n n) a))
  (define (iter n i)
    (cond ((= i n) true)
          ((fermat-test n i) (iter n (+ i 1)))
          (else false)))
  (iter n 2))

(newline)
(newline)
(display "Solutin 1.27")
(newline)

(carmichael? 1000)
(carmichael? 10000)
(carmichael? 1000000)


;; Solution 1.28
(define (expmod-new base exp m)
  (define (nontrivial-test x n)
    (if (and (not (or (= x 1)
                      (= x (- n 1))))
         (= (remainder (square x)
                       n)
            1))
        0
        x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (nontrivial-test (expmod-new base (/ exp 2) m) m))
                    m))
        (else
         (remainder (* base (expmod-new base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (define (iter a)
    (cond ((= a 0)
           #t)
          ((try-it (+ 1 (random (- n 1))))
           (iter (- a 1)))
          (else #f)))
  (iter 10))
