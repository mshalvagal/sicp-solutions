;basic leaf definition
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? x) (eq? 'leaf (car x)))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

;helper functions - appending two lists and element testing
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;basic tree definitions
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree) 
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;message decoding
(define (decode bits tree)
  (define (decode-iter bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-iter (cdr bits) tree))
              (decode-iter (cdr bits) next-branch)))))
  (decode-iter bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;helper functions for making an ordered set of leaves
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;testing our decoder
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(display (decode sample-message sample-tree)) (newline)
(define decoded-message (decode sample-message sample-tree))

;message encoding
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons '0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons '1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))

(display (encode decoded-message sample-tree)) (newline)

;Huffman tree generation from weighted symbol list
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                                    (cddr pairs)))))

;Encoding example
(define song-pairs
  '((a    2) (na  16)
    (boom 1) (sha  3)
    (get  2) (yip  9)
    (job  2) (wah  1)))
    
(define song
  '(get a job
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip yip yip yip yip yip
    sha boom))
    
(define song-tree
  (generate-huffman-tree song-pairs))
(define coded-song (encode song song-tree))

(newline)
(display song) (newline)
(display coded-song) (newline)
(display (length coded-song)) (newline)
(display (* 3 (length song))) (newline)


