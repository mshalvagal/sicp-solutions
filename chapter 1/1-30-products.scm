(define (prod term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod term (next a) next b))))

(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

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
