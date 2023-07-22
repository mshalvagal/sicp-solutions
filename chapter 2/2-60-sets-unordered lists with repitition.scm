; if we allow repeated elements in our list representation of sets, 
; building sets becomes easier, but search becomes unnecessarily expensive

;O(n*)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;O(1)
(define (adjoin-set x set)
  (cons x set))

;O(n^2*)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;O(n)
(define (union-set set1 set2)
  (define (union-iter union set1 set2)
    (if (null? set1)
        union
        (union-iter (cons (car set1) union)
                    (cdr set1)
                    set2)))
  (union-iter set2 set1 set2))

(display (adjoin-set 1 (list 4 2 3))) (newline)
(display (intersection-set (list 1 2 3) (list 2 4 5 3))) (newline)
(display (union-set (list 1 2 3) (list 2 4 5 3))) (newline)
