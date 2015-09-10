#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

;; Solution 2.33

(define (map p sequence)
  (accumulate (lambda (x y)  (cons (p x ) y))
              null sequence))

(define (append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ y)) 0 sequence))

;; Solution 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coefficient higher-terms)
     (+ this-coefficient (* higher-terms x)))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


;; Solution 2.35
(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (x)
          (if (not (pair? x))
              1
              (count-leaves x))) t)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
(count-leaves (list x x))



;; Solution 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init
                        (map (lambda(x)
                               (car  x))
                             seqs))
            (accumulate-n op init (map (lambda (x)
                                         (cdr x))
                                       seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))


;; Solution 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v))
       m))

(define (transpose mat)
  (accumulate-n
   cons
   null
   mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (x)
       (map (lambda (y)
              (dot-product x y))
            cols))
     m)))


;; Solution 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-left  / 1 (list 1 2 3))
(fold-left  list null (list 1 2 3))


;; Solution 2.39
(define (reverse sequence)
  (fold-left
   (lambda (x y) (cons y x))
   null
   sequence))

(reverse (list 1 2 3 4 5))
