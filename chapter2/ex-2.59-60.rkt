#lang scheme

;; Solution 2.59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x)
         true)
        (else
         (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set
                (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((not (element-of-set? (car set1) set2))
         (union-set (cdr set1) (cons (car set1) set2)))
        (else
         (union-set (cdr set1) set2))))


;; Solution 2.60

(define (adjoin-set-new x set)
      (cons x set))

(define (union-set-new set1 set2)
  (append set1 set2))
