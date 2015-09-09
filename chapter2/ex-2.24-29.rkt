#lang scheme


(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                 [count-leaves (cdr items)]))))


;;Solution 2.24
(count-leaves (list 1 (list 2 (list 3 4))))

;; Solution 2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

(car (car '((7))))


(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (6 7)))))))))))))))


;; Solution 2.26
(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)

(cons x y)

(list x y)

;; Solution 2.27
(define lis
  (list (list 1 2) (list 3 4)))

(define (reverse li)
  (if (null? li)
      null
      (append (reverse (cdr li)) (list (car li)))))

(define (deep-reverse z)
  (cond ((null? z) '())
        ((pair? (car z))
         (append (deep-reverse (cdr z))
               (list (deep-reverse (car z)))))
        (else
         (append (deep-reverse (cdr z))
                 (list (car z))))))

(deep-reverse lis)

;; Solution 2.28
(define (fringe items)
  (define (iter items res)
     (cond ((null? items) res )
           ((not (pair? items))
            (append res (list items)))
           (else
            (append (fringe (car items))
                    (fringe (cdr items))))))
  (iter items '()))



(fringe (list lis lis))


;; Solution 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;-------- a ---------
(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

;-------- b --------
(define (mobile? x)
  (pair? (car x)))

(define (weight? x)
  (and (not (mobile? x))
       (not (pair? (branch-structure x)))))

(define (total-weight x)
  (cond ((weight? x) (branch-structure x))
        ((mobile? x)
         (+ (total-weight (left-branch x))
            (total-weight (right-branch x))))
        (else
         (total-weight (branch-structure x)))))

; test total-weight
(define m1 (make-mobile (make-branch 1 48)
                        (make-branch 4 12)))
(total-weight m1)
;Value: 60

(define m2 (make-mobile (make-branch 6 m1)
                        (make-branch 9 40)))
(total-weight m2)
;Value: 100

(define m3 (make-mobile (make-branch 3 140)
                        (make-branch 7 60)))
(total-weight m3)
;Value: 200

(define m4 (make-mobile (make-branch 20 m2)
                        (make-branch 10 m3)))
(total-weight m4)
;Value:: 300

;-------- c --------
(define (balanced? x)
  (if (mobile? x)
      (let ((lb (left-branch x))
            (rb (right-branch x)))
        (and (= (* (total-weight lb) (branch-length lb))
                (* (total-weight rb) (branch-length rb)))
; should be (balanced? (branch-structure lb/rb)) instead of (balanced? lb/rb)
; see comment by shen
             (balanced? lb)
             (balanced? rb)))
      true))

; test
(balanced? m1)
;Value: #t

(balanced? m2)
;Value: #t

(balanced? m3)
;Value: #t

(balanced? m4)
;Value: #t

(define m3u (make-mobile (make-branch 3 140)
                         (make-branch 7 63)))
(balanced? m3u)
;Value: #f

(define m4u (make-mobile (make-branch 20 m2)
                         (make-branch 10 m3u)))
(balanced? m4u)
;Value: #f

;-------- d --------
(define (make-mobile-new left right)
  (cons left right))
(define (make-branch-new length structure)
  (cons length structure))

(define (right-branch-new m)
  (cdr m))

(define (branch-structure-new b)
  (cdr b))
