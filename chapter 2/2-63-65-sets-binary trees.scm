(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        (else (make-tree (entry set)
                         (left-branch set)
                         (adjoin-set x (right-branch set))))))

;the other function also produces the same result
;but the alternate implementation calls the append function which is O(log n),
;because the list size going into append is cut in half every call
;making the overall complexity O(n*log n)
;this (second) implementation grows slower as O(n) (visits every node once)
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elms n)
  (if (= n 0)
      (cons '() elms)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elms left-size)))
          (let ((left-tree (car left-result))
                (non-left-elms (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elms))
                  (right-result (partial-tree (cdr non-left-elms) 
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elms (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elms))))))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set-list (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set-list (cdr set1) set2))
              ((< x2 x1) (intersection-set-list set1 (cdr set2)))))))

(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set-list (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set-list (cdr set1) set2)))
                      (else (cons x2 (union-set-list set1 (cdr set2)))))))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list set1)
                                     (tree->list set2))))
(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list set1)
                              (tree->list set2))))


(define set1 (list->tree (list 1 2 3 4 5 6)))
(define set2 (list->tree (list 4 5 6 7 8 9)))

(display set1) (newline)
(display (tree->list set1)) (newline)(newline)

(display set2) (newline)
(display (tree->list set2)) (newline)(newline)

(display (intersection-set set1 set2)) (newline)(newline)

(display (union-set set1 set2)) (newline)
(display (tree->list (union-set set1 set2))) (newline)
