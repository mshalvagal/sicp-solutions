#lang sicp
(#%require "streams.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt integrator)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integrator (delay dy) y0 dt))
    (set! dy (stream-map f y))
  y))
(stream-ref (solve (lambda (y) y) 1 0.001 integral) 1000)


(newline)
(display "Exercise 3.77")(newline)
(define (integral-gen delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral-gen (delay (stream-cdr integrand))
                                   (+ (* dt (stream-car integrand))
                                      initial-value)
                                   dt)))))
(stream-ref (solve (lambda (y) y) 1 0.001 integral-gen) 1000)

(newline)
(display "Exercise 3.78")(newline)
(define (solve-2nd a b y0 dy0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*)
        (ddy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
    y))

; Some test cases to verify an implementation..   
(stream-ref (solve-2nd 1 0 1 1 0.0001) 10000)  ; e                                                         
(stream-ref (solve-2nd 0 -1 1 0 0.0001) 10472)  ; cos pi/3 = 0.5                                           
(stream-ref (solve-2nd 0 -1 0 1 0.0001) 5236)  ; sin pi/6 = 0.5


(newline)
(display "Exercise 3.79")(newline)
(define (generalized-solve-2nd f y0 dy0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*)
        (ddy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (stream-map f (scale-stream dy (/ 1.0 dt)) y))
    y))
; Some test cases to verify an implementation..   
(stream-ref (generalized-solve-2nd (lambda (dy y) y) 1 1 0.0001) 10000)  ; e
(stream-ref (generalized-solve-2nd (lambda (dy y) (* y -1)) 1 0 0.0001) 10472)  ; cos pi/3 = 0.5
(stream-ref (generalized-solve-2nd (lambda (dy y) (* y -1)) 0 1 0.0001) 5236)  ; sin pi/6 = 0.5


(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (let ((vC '*unassigned*)
          (iL '*unassigned*)
          (dvC '*unassigned*)
          (diL '*unassigned*))
      (set! vC (integral (delay dvC) vC0 dt))
      (set! iL (integral (delay diL) iL0 dt))
      (set! diL (add-streams (scale-stream vC (/ 1 L))
                               (scale-stream iL (/ (* R -1) L))))
      (set! dvC (scale-stream iL (/ -1 C)))
      (cons vC iL))))