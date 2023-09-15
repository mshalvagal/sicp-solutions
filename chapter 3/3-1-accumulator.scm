#lang sicp

(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ sum x))
    sum))

(define A1 (make-accumulator 10))
(A1 25)
(A1 15)
(A1 -100)