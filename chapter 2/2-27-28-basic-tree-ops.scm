(define (reverse-lis lis)
  (define (rev-iter lis rev-lis)
    (if (null? lis)
        rev-lis
        (rev-iter (cdr lis) (cons (car lis) rev-lis))))
  (rev-iter lis '()))

(define (deep-reverse lis)
  (define (rev-iter l rev-lis)
    (cond ((null? l) rev-lis)
          ((not (pair? (car l))) (rev-iter (cdr l) (cons (car l) rev-lis)))
          (else (rev-iter (cdr l) (cons (deep-reverse (car l)) rev-lis)))))
  (rev-iter lis '()))

(define (fringe lis)
  (cond ((null? lis) '())
        ((not (pair? (car lis))) (cons (car lis) (fringe (cdr lis))))
        (else (append (fringe (car lis)) (fringe (cdr lis))))))

(define x (list (list 1 2) (list 3 4) (list 5 6)))
(define y (list (list 1 2) (list 3 4 (list 5 6))))

(display x) (newline)
(display (reverse-lis x)) (newline)
(display (deep-reverse x)) (newline)
(display (fringe x)) (newline)
(newline)
(display y) (newline)
(display (fringe y)) (newline)
