#lang scheme

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-Branch tree) (car tree))
(define (right-Branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-Branch branch))
        ((= bit 1) (right-Branch branch))
        (else (error "bad bit:
               CHOOSE-BRANCH" bit))))

(define (adjoin-Set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-Set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-Set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;; SOlution 2.67
(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;; Value (A D A B B C A)

;; Solution 2.68
(define (encode-symbol sym tree)
  (define (has-symbol? sym branch)
    (member sym (symbols branch)))
  (if (leaf? tree)
      null
      (let ((left  (left-Branch  tree))
            (right (right-Branch tree)))
        (cond ((has-symbol? sym left)  (cons 0 (encode-symbol sym left)))
              ((has-symbol? sym right) (cons 1 (encode-symbol sym right)))
              (else (error "symbol not in tree" sym))))))


(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(equal? (encode (decode sample-message sample-tree)
                sample-tree)
        sample-message)


;; Solution 2.69

(define (successive-merge pairs)
  (if (= (length pairs) 1)
      (car pairs)
      (successive-merge
      (adjoin-Set (make-code-tree (car pairs)
                                        (cadr pairs))
                        (cddr pairs)))))

(define test-pairs '((A 4) (B 2) (C 1) (D 1)))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(equal? sample-tree
        (generate-huffman-tree test-pairs))
