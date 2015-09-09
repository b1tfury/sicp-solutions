#lang scheme
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))

;; Solution 2.21

(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list-new items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))

(square-list-new (list 1 2 3 4 ))


;; Solution 2.22

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(square-list-iter (list 1 2 3 4))

(define (square-list-iter-new items)
    (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(square-list-iter-new (list 1 2 3 4))


;; Solution 2.23
(define (for-each proc items)
  (cond((null? items) true)
       ((proc (car items))
       (for-each proc (cdr items)))))

(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))
