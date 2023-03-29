(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seq-of-seq)
  (if (null? seq-of-seq)
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seq-of-seq))
            (accumulate-n op init (if (null? (cdar seq-of-seq))
                                      '()
                                      (map (lambda (x) (cdr x)) seq-of-seq))))))

(display (map (lambda (x) (car x)) (list (list 1 2 3) (list 4 5 6)))) (newline)
(display (map (lambda (x) (cdr x)) (list (list 1 2 3) (list 4 5 6)))) (newline)

(display (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))) (newline)
(newline)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define v (list 1 2 3))
(define m (list (list 1 2 3) (list 2 4 6)))
(define n (list (list 1) (list 1) (list 1)))

(display (dot-product v v)) (newline)
(display (matrix-*-vector m v)) (newline)
(display (transpose m)) (newline)
(display (matrix-*-matrix m n)) (newline)
