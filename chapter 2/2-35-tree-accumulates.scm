(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves tree)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (enumerate-tree tree)))

(display (count-leaves (list 1 3 (list 0 (list 5 0)) (list 1))))
