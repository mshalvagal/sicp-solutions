#lang sicp

(define (make-f state)
  (lambda (x)
    (set! state (* state x))
    state))

(define ftest1 (make-f 1))
(define ftest2 (make-f 1))
(define f1 (make-f 1))
(define f2 (make-f 1))

; first see what happens for each call if it were to be the first
(ftest1 0)
(ftest2 1)

; now for the two different results for different orders of execution
(+ (f1 0) (f1 1))
(+ (f2 1) (f2 0))