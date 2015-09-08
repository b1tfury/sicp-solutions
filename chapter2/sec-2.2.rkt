#lang scheme

;; Hierarchical data and the Closure Property

;; Representing Sequences

(define one-through-four (list 1 2 3 4))

(car one-through-four)

(cdr one-through-four)

(car (cdr (cdr one-through-four)))
(cons 10 one-through-four)

;; List operations

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares
  (list 1 4 9 16 25 ))

(list-ref squares 3)


(define (length items)
  (if (null? items)
      0
      ( + 1 (length (cdr items)))))

(length squares)

(define (length-new items)
  (define (length-iter items counter)
    (if (null? items)
        counter
        (length-iter (cdr items) (+ counter 1))))
  (length-iter items 0))

(length-new squares)

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1)
                    l2))))
(append squares squares)
