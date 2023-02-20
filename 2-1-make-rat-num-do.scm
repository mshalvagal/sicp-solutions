(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d))
        (denom-sign (if (< d 0)
                        -1
                        1)))
    (cons (* denom-sign (/ n g)) (* denom-sign (/ d g)))))

(print-rat (make-rat 6 9))
(print-rat (make-rat -6 -9))
(print-rat (make-rat -6 9))
(print-rat (make-rat 6 -9))
