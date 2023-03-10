(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (car-iter x count)
    (if (= (remainder x 2) 0)
        (car-iter (/ x 2) (+ count 1))
        count))
  (car-iter z 0))

(define (cdr z)
  (define (cdr-iter x count)
    (if (= (remainder x 3) 0)
        (cdr-iter (/ x 3) (+ count 1))
        count))
  (cdr-iter z 0))

(display (cons 1 2)) (newline)
(display (car (cons 1 2))) (newline)
(display (cdr (cons 1 2))) (newline)
