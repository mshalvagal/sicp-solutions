#lang sicp

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(let ((m (make-mutex)))
  (display (m 'acquire))
  (display (m 'release))
  (display (m 'acquire)))

(define (make-semaphore_v1 n)
  (let ((count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (count < n)
                 