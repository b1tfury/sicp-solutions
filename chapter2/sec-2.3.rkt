#lang scheme

;; Symbolic data

;; Quotation
(define a  1)

(define b 2)


(list 'a b)


(define (memeq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else
         (memeq item (cdr x)))))

(memeq 4 (list 1 2 3 4 5 5 43 3 43 ))

;; Example Symbolic Differentiation
