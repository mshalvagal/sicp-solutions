#lang sicp
;"random" number generator 
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define (make-rand init-seed)
  (let ((x init-seed))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
               x))
            ((eq? m 'reset)
             (lambda (new-seed)
               (set! x new-seed)
               (display "Resetting seed")
               (newline)))
            (else error("Invalid method"))))))

(define rand (make-rand 42))

(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 42)
(rand 'generate)
(rand 'generate)
(rand 'generate)
