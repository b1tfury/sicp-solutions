#lang scheme

(define (exp a b)
  (cond ((= b 0) 1)
        (* a (exp a (- b 1)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation e1 e2)
  (cond ((= e2 0) 1)
        ((= e2 1) e1)
        ((and (number? e1) (number? e2))
         (exp e1 e2))
        (else (list '** e1 e2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (apply make-sum (cddr s)))

(define (product? x)
(and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (apply make-product (cddr p)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
          (make-product
           (multiplier expr)
           (deriv (multiplicand expr) var))
          (make-product
           (deriv (multiplier expr) var)
           (multiplicand expr))))
        ((exponentiation? expr)
         (make-product
          (exponent expr)
          (exp (base expr) (- (base expr) 1))
          (deriv (base expr) var)))
        (else (error "unknown expression
                      type: DERIV" exp)))

         )


(deriv '(* x y (+ x 3)) 'x)
