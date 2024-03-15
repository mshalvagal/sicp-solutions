#lang sicp
(#%require "streams.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (integral (scale-stream i (/ 1 C)) v0 dt)
     (scale-stream i R))))

(define RC1 (RC 5 1 0.5))

(newline)
(display "Exercise 3.74")(newline)
(define sense-data
  (cons-stream 1
               (cons-stream 1
                            (cons-stream -1
                                         (cons-stream -1
                                                      sense-data)))))

(display "Input stream")(newline)
(display-stream (finite-wrapper sense-data 10))
(define (sign-change-detector new old)
  (if (>= old 0)
      (< new 0)
      (>= new 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings-alyssa (make-zero-crossings sense-data 0))
(newline)
(display "Zero crossings - Alyssa P Hacker")(newline)
(display-stream (finite-wrapper zero-crossings-alyssa 10))

(define zero-crossings-eva
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(newline)
(display "Zero crossings - Eva Lu Ator")(newline)
(display-stream (finite-wrapper zero-crossings-eva 10))

(define (make-zero-crossings-lem input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings-lem (stream-cdr input-stream)
                                          (stream-car input-stream)
                                          avpt))))
(define zero-crossings-lem (make-zero-crossings-lem sense-data -1 -1))
(newline)
(display "Exercise 3.75")(newline)
(display "Zero crossings - Lem E Tweakit & Louis Reasoner (corrected)")(newline)
(display-stream (finite-wrapper zero-crossings-lem 10))

(newline)
(display "Exercise 3.76")(newline)
(define (smooth stream)
  (cons-stream (/ (+ (stream-car stream)
                     (stream-car (stream-cdr stream)))
                  2)
               (smooth (stream-cdr stream))))
(display "Smoothed input stream")(newline)
(display-stream (finite-wrapper (smooth sense-data) 10))
(newline)
(display "Zero crossings of smoothed stream - og Alyssa P Hacker")(newline)
(display-stream (finite-wrapper (make-zero-crossings (smooth sense-data) 0) 10))