(define (cont-frac n d k)
  (define (f i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (f (+ i 1))))))
  (f 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k (/ (n k) (d k))))

(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    12))

(newline)

(display (cont-frac-iter (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         12))

(newline)
(newline)

(define (euler-d i)
  (if (= (modulo i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))

(display (+ 2 (cont-frac (lambda (i) 1.0) euler-d 12)))

(newline)
(newline)

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x -1)))
             (lambda (i) (- (* 2.0 i) 1))
             k))

(define x 1)
(display (tan-cf x 8)) (newline)
(display (tan x)) (newline)
