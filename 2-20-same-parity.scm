(define (same-parity . z)
  (define (make-parity-lis parity lis)
    (if (null? lis)
        '()
        (let ((cur-elem (car lis)))
          (if (= (remainder cur-elem 2) parity)
              (cons cur-elem (make-parity-lis parity (cdr lis)))
              (make-parity-lis parity (cdr lis))))))
  (cons (car z) (make-parity-lis (remainder (car z) 2) (cdr z))))


(display (same-parity 1 2 3 4 5 6 7)) (newline)
(display (same-parity 2 3 4 5 6 7)) (newline)
