#lang scheme

(define lis (list 1 2 3 4 5))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1)
                    l2))))

;; Solution 2.17
(define (last-pair items)
  (cond ((null? items)
         error "list is empty")
        ((null? (cdr items))
         (car items))
        (else
         (last-pair (cdr items)))))

(last-pair lis)


;; Solution 2.18
(define (reverse li)
  (if (null? li)
      '()
      (append (reverse (cdr li)) (list (car li)))))

(reverse lis)



;; Solution 2.20
(newline)
(display "Solution 2.20")
(newline)
(define (same-parity i . li)
  (define (sp x result)
    (cond ((null? x) result)
          ((even? (- i (car x)))
           (sp (cdr x) (append result (list (car x)))))
          (else
           (sp (cdr x) result))))
  (sp li (list i)))

(same-parity 1 2 3 4 5 6)
