(define (last-pair lis)
  (if (null? (cdr lis))
      lis
      (last-pair (cdr lis))))

(define (reverse lis)
  (define (rev-iter lis rev-lis)
    (if (null? lis)
        rev-lis
        (rev-iter (cdr lis) (cons (car lis) rev-lis))))
  (rev-iter lis '()))

(display (last-pair (list 1 2 3 4 5))) (newline)
(display (reverse (list 1 2 3 4 5))) (newline)
