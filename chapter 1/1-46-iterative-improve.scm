#lang sicp
(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (let ((next-guess (improve-guess guess)))
      (display next-guess)
      (newline)
      (if (good-enough? guess next-guess)
          next-guess
          (iter next-guess))))
  (lambda (guess) (iter guess)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (next-guess guess) (f guess))
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? next-guess) first-guess))

(define (sqrt x)
  (fixed-point (lambda(y) (average y (/ x y))) 1))

(fixed-point (lambda (x) (* 0.5 (+ x (/ (log 1000) (log x))))) 2)
(newline)

(sqrt 2)
