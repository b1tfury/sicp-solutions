#lang scheme

(define (memeq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else
         (memeq item (cdr x)))))

;; Solution 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))


;; Solution 2.54
(define (equal? a b)
  (cond ((and (null? a) (null? b)) true)
        ((eq? (car a) (car b))
         (equal? (cdr a) (cdr b)))
        (else false)))

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

;; Solution 2.55

(car ''abracadabra)

;; this will return "quote" since that is the first element
