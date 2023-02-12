(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (prod term a next b)
  (accumulate * 1 term a next b))

(define (prod-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (square x) (* x x))

(define (inc x) (+ 1 x))

(define (factorial n)
  (define (id x) x)
  (prod id 1 inc n))

(display (factorial 3)) (newline)
(display (factorial 4)) (newline)
(display (factorial 5)) (newline)

(newline)

(define (pi-prod n)
  (define (wallis-term x)
    (* (/ (* 2 x) (- (* 2 x) 1)) (/ (* 2 x) (+ (* 2 x) 1))))
  (* 2.0 (prod-iter wallis-term 1 inc n)))

(display (pi-prod 10)) (newline)
(display (pi-prod 100)) (newline)
(display (pi-prod 1000)) (newline)

(newline)

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;(display (* 8 (pi-sum 1 1000)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x)
    (+ (* 2 h) x))
  (* (/ h 3) 
     (+ (f a)
        (f b)
        (* 4 (sum-iter f (+ a h) add-2h b))
        (* 2 (sum-iter f (+ a (* 2 h)) add-2h (- b h))))))

(define (cube x) (* x x x))

(display (integral cube 0 1 0.1)) (newline)
(display (integral cube 0 1 0.01)) (newline)
(display (integral cube 0 1 0.001)) (newline)

(newline)

(display (integral-simpson cube 0 1.0 10)) (newline)
(display (integral-simpson cube 0 1.0 100)) (newline)
(display (integral-simpson cube 0 1.0 1000)) (newline)
