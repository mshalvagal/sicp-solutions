(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(display (((double (double double)) inc) 5))
(newline)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(display ((compose square inc) 6))
(newline)

(define (repeated f n)
  (define (iter i result)
    (let ((composition (compose f result)))
      (if (= i n) result (iter (+ i 1) composition))))
  (iter 1 f))

(display ((repeated square 2) 5))
(newline)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (extra-smooth f n)
  (repeated (smooth f) n))

