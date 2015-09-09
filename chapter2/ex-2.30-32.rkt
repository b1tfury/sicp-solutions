#lang scheme

(define (square x) (* x x))
;; Solution 2.30
(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree))
         (* tree tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square-tree-new tree)
  (map (lambda (tree)
         (if (pair? tree)
             (square-tree-new tree)
             (* tree tree)))
       tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


(square-tree-new
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))



;; Solution 2.31
(define (tree-map proc tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         (proc tree))
        (else
         (cons (tree-map proc (car tree))
               (tree-map proc (cdr tree))))))

(define (square-tree-abs tree)
  (tree-map square tree))

(square-tree-abs
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


;; Solution 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (append (list (car s)) x))
                          rest)))))

(subsets (list 1 2 3))
