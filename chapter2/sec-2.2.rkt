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


;; Mapping over list

(define (map proc lists)
  (if (null? lists)
      null
      (cons (proc (car lists))
            (map proc (cdr lists)))))


(map (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (scale-list items factor )
  (map (lambda (x) (* x factor))
       items))

(scale-list squares 10)

;; Hierarchical Structures

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                 [count-leaves (cdr items)]))))

(define x (cons (list 1 2) (list 3 4)))

(length x)

(count-leaves x)

;; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree (car tree)
                           factor)
               (scale-tree (cdr tree)
                           factor)))))

(define (scale-tree-new tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-new sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7))
            10)
(scale-tree-new (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7))
            10)



;; Sequences as Conventional Interfaces
;;Sequence operations

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5 6 7))

(define a (accumulate cons null (list 1 2 3 4 5)))

(car a)
(cdr a)


(define (enumerate low high)
  (if (> low high)
      null
      (cons low
            (enumerate
             (+ low 1)
             high))))

(enumerate 1 10)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree))
         (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))


(define (square x) (* x x))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (sum-odd-squares tree)
  (accumulate
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
   cons
   null
   (filter even?
           (map fib
                (enumerate 0 n)))))


(define (list-fib-squares n)
  (accumulate
   cons
   null
   (map square
        (map fib
             (enumerate 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-odd-elements items)
  (accumulate
   *
   1
   (map square
        (filter odd? items))))

(product-of-squares-odd-elements (list 1 2 3 4 5))
