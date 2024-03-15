#lang sicp
(#%require "streams.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (square x) (* x x))
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(display "Original interleaved pairs i<=j")(newline)
(display-stream (finite-wrapper (pairs integers integers) 10))


(newline)
(display "Exercise 3.67")(newline)
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave (interleave
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t)))
                            (stream-cdr s)))
                (all-pairs (stream-cdr s) (stream-cdr t)))))
(display "All interleaved pairs i,j")(newline)
(display-stream (finite-wrapper (all-pairs integers integers) 10))


(newline)
(display "Exercise 3.68")(newline)
;(define (lr-pairs s t)
;  (interleave
;   (stream-map (lambda (x) (list (stream-car s) x))
;               t)
;   (lr-pairs (stream-cdr s) (stream-cdr t))))
(display "Louis reasoner pairs functiongoes into infinite recursion because interleave is a normal procedure.")(newline)
(display "So all its arguments including the recursive pairs call need to be evaluated first, ad infinitum.")(newline)
;(display-stream (finite-wrapper (lr-pairs integers integers) 100))


(newline)
(display "Exercise 3.69")(newline)
;could make an optimized triples generator calculating pairs(t,u) only once, but the below is easier to understand.
;also i'm too lazy to do it right now
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
(display "Ordered interleaved triples i<=j<=k")(newline)
(display-stream (finite-wrapper (triples integers integers integers) 10))
(newline)
(display "Pythagorean triples")(newline)
(define pythag-triples
  (stream-filter (lambda(x)
                   (= (+ (square (car x)) (square (cadr x))) (square (caddr x))))
                 (triples integers integers integers)))

(display-stream (finite-wrapper pythag-triples 4))

(newline)
(display "Exercise 3.70")(newline)
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (< (weight s1car) (weight s2car))
               (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    weight)))

(define (weight-a x)
  (+ (car x) (cadr x)))
(define (weight-b x)
  (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x))))

(display "Part a - ordered pairs i<=j")(newline)
(display-stream (finite-wrapper (weighted-pairs integers integers weight-a) 10))

(newline)
(display "Part b - ordered pairs i<=j")(newline)
(define (is-not-divisible? x)
  (and (> (remainder x 2) 0)
       (> (remainder x 3) 0)
       (> (remainder x 5) 0)))
(define indivisible-integers (stream-filter is-not-divisible? integers))
(display-stream (finite-wrapper (weighted-pairs indivisible-integers indivisible-integers weight-b) 20))

(newline)
(display "Exercise 3.71")(newline)
(display "This was a neat exercise. First I generated pairs weighted by sum of cubes")(newline)
(display "Then I created a stream of the weights (sums of cubes), made another stream consisting of pairs of consecutive weights.")(newline)
(display "Finally I filtered for equal elements in the pairs to get the Ramanujan numbers below.")(newline)
(define (cube x)
  (* x x x))
(define (ramanujan-weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (cube i) (cube j))))
(define ramanujan-weighted-integer-pairs (weighted-pairs integers integers ramanujan-weight))
(define ramanujan-weights (stream-map ramanujan-weight ramanujan-weighted-integer-pairs))
(define (consecutive-elem-pairs-stream stream)
  (cons-stream (list (stream-car stream)
                     (stream-car (stream-cdr stream)))
               (consecutive-elem-pairs-stream (stream-cdr stream))))
(define (equal-elems? x)
  (= (car x) (cadr x)))
(define ramanujan-numbers (stream-map (lambda(x) (car x))
                                      (stream-filter equal-elems?
                                                     (consecutive-elem-pairs-stream ramanujan-weights))))
(display-stream (finite-wrapper ramanujan-numbers 6))


(newline)
(display "Exercise 3.72")(newline)
(display "Super dirty-looking solution below, but hey, it works.")(newline)
(define (sum-sq-weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (square i) (square j))))
(define sum-sq-weighted-integer-pairs (weighted-pairs integers integers sum-sq-weight))
(define sum-sq-triples (stream-map (lambda(x) (list (sum-sq-weight x) x)) sum-sq-weighted-integer-pairs))
(define (consecutive-elem-triples-stream stream)
  (cons-stream (list (stream-car stream)
                     (stream-car (stream-cdr stream))
                     (stream-car (stream-cdr (stream-cdr stream))))
               (consecutive-elem-triples-stream (stream-cdr stream))))
(define (equal-sum-sqs-3elems? x)
  (let ((triple1 (car x))
        (triple2 (cadr x))
        (triple3 (caddr x)))
    (let ((score1 (car triple1))
          (score2 (car triple2))
          (score3 (car triple3)))
      (and (= score1 score2) (= score2 score3)))))
(display-stream (finite-wrapper (stream-map (lambda(x) (list (caar x) (cadr (car x)) (cadr (cadr x)) (cadr (caddr x))))
                                            (stream-filter equal-sum-sqs-3elems?
                                                           (consecutive-elem-triples-stream sum-sq-triples)))
                                5))


