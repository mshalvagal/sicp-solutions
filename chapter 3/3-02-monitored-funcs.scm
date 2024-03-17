#lang sicp

(define (make-monitored f)
  (let ((count 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls?)
             count)
            ((eq? m 'reset-count)
             (set! count 0))
            (else
             (begin (set! count (+ count 1))
                    (f m)))))))

(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)
(newline)

(s 256)
(s 10)
(s 'how-many-calls?)
(newline)

(s 'reset-count)
(s 'how-many-calls?)
