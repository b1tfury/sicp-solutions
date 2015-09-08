#lang scheme

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound x))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

;; Solution 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound x) (min (car x) (cdr x)))

(define (upper-bound x) (max (car x) (cdr x)))


 ;; Solution 2.8
(define (sub-interval x y)
  (add-interval x
                (make-interval (- upper-bound y)
                               (- lower-bound y))))


;; Solution 2.9
(define (sum-width x y)
  (let ((width (add-interval x y)))
    (/ (- (upper-bound width) (lower-bound width)) 2)))


;; Solution 2.10
(define (div-interval-new x y)
  (cond ((= (upper-bound x) (lower-bound x) error "zero span interval"))
        ((mul-interval x
                       (make-interval
                        (/ 1.0 (upper-bound y))
                        (/ 1.0 (lower-bound y)))))))


;; Solution 2.11
(define (mul-interval-new x y)
  (let* ((x-up (upper-bound x))
         (x-lo (lower-bound x))
         (y-up (upper-bound y))
         (y-lo (lower-bound y))
         (x-sign (cond ((and (< x-lo 0) (< x-up 0)) -1)
                       ((and (< x-lo 0) (> x-up 0)) 0)
                       (else 1)))
         (y-sign (cond ((and (< y-lo 0) (< y-up 0)) -1)
                       ((and (< y-lo 0) (> y-up 0)) 0)
                       (else 1))))
    (cond ((< x-sign 0)
           (cond ((< y-sign 0) ; (- -) * (- -)
                  (make-interval (* x-up y-up)
                                 (* x-lo y-lo)))
                 ((= y-sign 0) ; (- -) * (- +)
                  (make-interval (* x-lo y-up)
                                 (* x-lo y-lo)))
                 (else         ; (- -) * (+ +)
                  (make-interval (* x-lo y-up)
                                 (* x-up y-lo)))))
          ((= x-sign 0)
           (cond ((< y-sign 0) ; (- +) * (- -)
                  (make-interval (* x-up y-lo)
                                 (* x-lo y-lo)))
                 ((= y-sign 0) ; (- +) * (- +)
                  (make-interval (min (* x-up y-lo) (* x-lo y-up))
                                 (max (* x-lo y-lo) (* x-up y-up))))
                 (else         ; (- +) * (+ +)
                  (make-interval (* x-lo y-up)
                                 (* x-up y-up)))))
          (else ; x: (+ +)
           (cond ((< y-sign 0) ; (+ +) * (- -)
                  (make-interval (* x-up y-lo)
                                 (* x-lo y-up)))
                 ((= y-sign 0) ; (+ +) * (- +)
                  (make-interval (* x-up y-lo)
                                 (* x-up y-up)))
                 (else         ; (+ +) * (+ +)
                  (make-interval (* x-lo y-lo)
                                 (* x-up y-up))))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))


;; Solution 2.12
(define (percent i)
  (* 100 (/ (width i) (center i))))

(define (make-center-percent cntr percnt)
  (make-interval (- cntr (/ percnt 100)) (+ cntr (/ percnt 100))))

;; Solution  2.13
(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))


;; Solution 2.14-2.16
;; this is happening because interval that appears multiple times in an expression will behave
;;independently though they should behave exactly the same way
