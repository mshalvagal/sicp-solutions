#lang sicp

(display (map (lambda (x) (* x x)) (list 1 2 3 4))) (newline)
(display (map (lambda (x y) (+ x y)) (list 1 2 3 4) (list 1 2 3 4))) (newline)

(define (for-each proc lis)
  (if (not (null? lis))
      (begin
       (proc (car lis))
       (for-each proc (cdr lis)))))

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))
