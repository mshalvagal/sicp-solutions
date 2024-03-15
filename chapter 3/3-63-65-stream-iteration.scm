#lang sicp
(#%require "streams.scm")

(define (average x1 x2)
  (/ (+ x1 x2) 2))
(define (square x)
  (* x x))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (let ((x1 (stream-car stream))
        (x2 (stream-car (stream-cdr stream))))
    (if (< (abs (- x1 x2)) tolerance)
        x2
        (stream-limit (stream-cdr stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(display "Exercise 3.64")(newline)
(display "Square root of 2 to 1 decimal place: ")
(sqrt 2 0.1)
(display "Square root of 2 to 5 decimal places: ")
(sqrt 2 0.00001)
(newline)


(define (partial-sums stream)
  (define sum
    (cons-stream (stream-car stream)
                 (add-streams sum (stream-cdr stream))))
    sum)
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


(display "Exercise 3.65")(newline)
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))
(display "ln2 raw series of approximations")(newline)
(display-stream (finite-wrapper ln2-stream 10))
(newline)
(display "ln2 euler transformed series of approximations")(newline)
(display-stream (finite-wrapper (euler-transform ln2-stream) 10))
(newline)
(display "ln2 accelerated series of approximations")(newline)
(display-stream (finite-wrapper (accelerated-sequence euler-transform ln2-stream) 10))