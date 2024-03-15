#lang sicp
(#%require "streams.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series series-stream)
  (stream-map / series-stream integers))

(display "Exercise 3.59")(newline)
(display "Part a - checking implementation of integrate-series. Should print 1/n upto n=10")(newline)
(display-stream (finite-wrapper (integrate-series ones) 10))


(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(display "Part b - checking implementation of sine-series")(newline)
(display-stream (finite-wrapper sine-series 10))


(newline)
(display "Exercise 3.60")(newline)
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                                         (scale-stream (stream-cdr s1) (stream-car s2)))
                            (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))
(display "Checking that sum of squared sine and cosine is 1. Output should be 1,0,0,0...")(newline)
(define sum-sqsinusoids (add-streams (mul-series sine-series sine-series)
                                     (mul-series cosine-series cosine-series)))
(display-stream (finite-wrapper sum-sqsinusoids 10))


(newline)
(display "Exercise 3.61")(newline)
(define (invert-unit-series s)
  (define inverse (cons-stream 1
                               (scale-stream (mul-series inverse
                                                         (stream-cdr s))
                                             -1)))
  inverse)
(display "Checking inverse of e^x, i.e, e^-x.")(newline)
(display-stream (finite-wrapper (invert-unit-series exp-series) 10))


(newline)
(display "Exercise 3.62")(newline)
(define (div-series num-series denom-series)
  (define (div-safe num-series denom-series)
    (mul-series num-series
                (invert-unit-series denom-series)))
  (let ((denom-const (stream-car denom-series)))
    (if (= denom-const 0)
        (error "Denominator has zero constant term")
        (div-safe (scale-stream num-series (/ 1 denom-const))
                  (scale-stream num-series (/ 1 denom-const))))))
(display "Checking that cosine/cosine is 1. Output should be 1,0,0,0...")(newline)
(display-stream (finite-wrapper (div-series cosine-series cosine-series) 10))
