#lang sicp
(#%require "streams.scm")


(display "Exercise 3.53")(newline)
(define s (cons-stream 1 (add-streams s s)))
(display "s should consist of powers of 2. Below should print 1,2,4,..1024")(newline)
(display-stream (finite-wrapper s 11))


(newline)
(display "Exercise 3.54")(newline)
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))
(display "1! upto 10! below")(newline)
(display-stream (finite-wrapper factorials 10))


(newline)
(display "Exercise 3.55")(newline)
(define (partial-sums stream)
  (define sum
    (cons-stream (stream-car stream)
                 (add-streams sum (stream-cdr stream))))
    sum)
(display-stream (finite-wrapper (partial-sums integers) 5))


(newline)
(display "Exercise 3.56")(newline)
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
(define S (cons-stream 1
                       (merge (scale-stream S 2)
                              (merge (scale-stream S 3)
                                     (scale-stream S 5)))))
(display-stream (finite-wrapper S 10))

