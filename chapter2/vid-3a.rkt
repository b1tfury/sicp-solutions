#lang scheme

(define (scale-list s l)
  (if (null? l)
      null
      (cons (* (car l) s)
            (scale-list s (cdr l)))))

(scale-list 10 (list 1 2 3 4 5))

(define (map proc l)
  (if (null? l)
      null
      (cons (proc (car l))
            (map proc (cdr l)))))


(define (scale-list-new s l)
  (map (lambda (x) (* x s)) l))

(scale-list-new 10 (list 1 2 3 4 5))

(define (square x) (* x x))
(map square (list 1 2 3 4 5))
