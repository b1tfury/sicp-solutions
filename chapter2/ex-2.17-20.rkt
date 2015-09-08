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
(define (same-parity . l)
  (let ((a (remainder (car l) 2)))
    (define (parity-iter items result)
       (cond ((null? items) result)
             ((= (remainder (car items) 2) a)
              (parity-iter (list (cdr items)) (append (result (list (car items))))))
             (else
              ((parity-iter (cdr items) (list (result)))
               (display items)))))
     (parity-iter (cdr l) '())))


(same-parity 1 2 3 4 5 6)
