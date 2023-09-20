#lang sicp

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P lower-x upper-x lower-y upper-y trials)
  (* (monte-carlo trials
                  (lambda ()
                    (P (random-in-range lower-x upper-x)
                       (random-in-range lower-y upper-y))))
     (- upper-x lower-x)
     (- upper-y lower-y)))

(define (in-unit-circle? x y)
  (< (+ (* x x) (* y y)) 1.0))

(estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 100000)
