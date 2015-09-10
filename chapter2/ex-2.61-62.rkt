#lang scheme

;; SOlution 2.61

(define (element-of-set-new? x set)
  (cond ((null? set)
         false)
        ((= x (car set) true))
        ((< x (car set) false))
        (else (element-of-set-new? x (cdr set)))))

(define (intersection-set-new set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-new
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set-new
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection-set-new
                          set1
                          (cdr set2)))))))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


;; Solution 2.62


(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
       '()
       (let ((x1 (car set1)) (x2 (car set2)))
         (cond ((= x1 x2)
                (cons x1
                      union-set (cdr set1) (cdr set2)))
                 ((< x1 x2)
                  (cons x1
                        (union-set (cdr set1)
                                   set2)))
                 ((> x1 x2)
                  (cons x2
                        (union-set set1
                                   (cdr set2))))))))
