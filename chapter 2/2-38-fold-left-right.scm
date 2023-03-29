(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(display (fold-right / 1 (list 1 2 3))) (newline)
(display (fold-left / 1 (list 1 2 3))) (newline)
(display (fold-right list '() (list 1 2 3))) (newline)
(display (fold-left list '() (list 1 2 3))) (newline)

(define (reverse sequence)
  (fold-right (lambda (x y)
                (cond ((not (pair? x)) (append y (list x)))
                      ((null? y) x)
                      (else (append y x))))
              '() 
              sequence))
(display (reverse (list 1 2 3 4 5 6))) (newline)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x))
              '() 
              sequence))
(display (reverse (list 1 2 3 4 5 6))) (newline)