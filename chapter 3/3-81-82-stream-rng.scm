#lang sicp
(#%require "streams.scm")

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define init-seed 42)
(define requests
  (cons-stream (cons 'generate 0)
               (cons-stream (cons 'generate 0)
                            (cons-stream (cons 'generate 0)
                                         (cons-stream (cons 'reset 42)
                                                      requests)))))


(newline)
(display "Exercise 3.81")(newline)
(define (request-handler request-stream rand-num-stream)
  (let ((req (stream-car request-stream))
        (x (stream-car rand-num-stream)))
    (cond ((eq? (car req) 'reset)
           (cons-stream (cdr req)
                        (request-handler (stream-cdr request-stream)
                                         (stream-cdr rand-num-stream))))
          ((eq? (car req) 'generate)
           (cons-stream (rand-update x)
                        (request-handler (stream-cdr request-stream)
                                         (stream-cdr rand-num-stream))))
          (else (error "Invalid request")))))

(define rand-nums
  (cons-stream init-seed
               (request-handler requests rand-nums)))

(display-stream (finite-wrapper rand-nums 10))


(newline)
(display "Exercise 3.82")(newline)
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (random-number-pairs low1 high1 low2 high2) 
         (cons-stream (cons (random-in-range low1 high1) (random-in-range low2 high2)) 
                                 (random-number-pairs low1 high1 low2 high2)))

(define (in-unit-circle? point)
  (let ((x (car point))
        (y (cdr point)))
    (< (+ (* x x) (* y y)) 1.0)))

(define (integral-estimator P lower-x upper-x lower-y upper-y)
  (stream-map (lambda (p) (* p (- upper-x lower-x) (- upper-y lower-y)))
              (monte-carlo (stream-map P
                                       (random-number-pairs lower-x upper-x lower-y upper-y))
                           0
                           0)))

(define pi-stream
  (integral-estimator in-unit-circle? -1.0 1.0 -1.0 1.0))

(stream-ref pi-stream 100000)