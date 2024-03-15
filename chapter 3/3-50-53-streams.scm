#lang sicp
(#%require "streams.scm")

(define (stream-enumerate-interval low high)
  (if (> low high)
      '()
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))


(display "Checking cons-stream implementation. Expected output: (1, #procedure) and 2")(newline)
(define a (cons-stream 1 2))
a
(stream-cdr a)

(newline)
(display "Checking stream-enumerate and display-stream implementation. Expected output: range(1,11)")(newline)
(display-stream (stream-enumerate-interval 1 10))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define stream_1 (stream-enumerate-interval 1 5))
(define stream_2 (stream-enumerate-interval 2 6))

(newline)
(display "Checking stream-map implementation (exercise 3.50). Expected output: (1+2, 2+3, 3+4, 4+5, 5+6)")(newline)
(display-stream (stream-map + stream_1 stream_2))

(define (show x)
  (display-line x)
  x)
(newline)
(display "Exercise 3.51")(newline)
(display "Expected output of line 1 - should print 0, and define x as (0, d(s-m(show (s-cdr (0, s-e-i 1 10))))")(newline)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(display "Expected output of line 2 - should print 0, 1, 2, 3, 4, 5 **5** because it executes show along the way")(newline)
(stream-ref x 5)
(display "Expected output of line 3 - should print 6, 7 **7** because it retrieves cached values for stream-cdr for elements upto 5 without executing show")(newline)
(stream-ref x 7)

(newline)
(display "Exercise 3.52")(newline)

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
;(display "Seq definition creates first element and waits, so seq=(1,#proc) and sum=1")(newline)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;(display "Value of sum=")(display sum) (newline)

;(display "y definition creates elements until first even and then waits, so seq=(1,3,6,#proc) and sum=6")(newline)
(define y (stream-filter even? seq))
;(display "Value of sum=")(display sum) (newline)

;(display "z definition creates elements until first multiple of 5 and then waits, so seq=(1,3,6,10,#proc) and sum=10")(newline)
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;(display "Value of sum=")(display sum) (newline)

(display "y stream-ref 7 creates elements until eighth even (0-indexing) and then waits, so seq=(1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,#proc) and sum=136")(newline)
(display "Should print 136")(newline)
(stream-ref y 7)
;(display "Value of sum=")(display sum) (newline)

(display "z display-stream creates all elements multiple of 5")(newline)
(display "Should print each element that is a multiple of 5")(newline)
(display-stream z)