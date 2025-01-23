#lang sicp

(define (require p)
  (if (not p) (amb)))

(let ((x (amb 1 2 3)))
    (require (>= x 2))
  x)

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(newline)
(display "Exercise 4.35")(newline)

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(display "A pythagorean triple between 4 and 12: ")
(a-pythagorean-triple-between 4 12)
(newline)(newline)

(display "Exercise 4.36")(newline)
(define (pythagorean-triples)
  (let ((k (an-integer-starting-from 2)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(display "A pythagorean triple: ")
(pythagorean-triples)
(newline)(newline)


(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(display "Solution to the multiple-dwelling puzzle: ")
(multiple-dwelling)
(newline)(newline)


(display "Exercise 4.39")(newline)
(define (multiple-dwelling-reordered-restrictions)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= cooper 1)))
    (require (> miller cooper))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= baker 5)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
(display "More optimized solution (reordered restrictions) to the multiple-dwelling puzzle: ")
(multiple-dwelling-reordered-restrictions)
(display "It is in general better to put the conditions corresponding to the last branching variables first so that failures can more quickly backtrack to the relevant level.")(newline)
(display "Note that the question says we can only change the order of the requires, not of the ambiguous variable let-definitions.")
(newline)(newline)(newline)


(display "Exercise 4.40")(newline)
(define (multiple-dwelling-optimized)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (let ((fletcher (amb 1 2 3 4 5)))
        (require (not (= fletcher 1)))
        (require (not (= fletcher 5)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require
              (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

(display "Fully optimized solution (interleaved branching and filtering) to the multiple-dwelling puzzle: ")
(multiple-dwelling-optimized)
(newline)(newline)


(display "Exercise 4.41")(newline)

(define (adjacent? x y)
  (= (abs (- x y)) 1))

(define (multiple-dwelling-ordinary)
  (define (display-soln b c f m s)
    (display (list (list 'baker b)
                   (list 'cooper c)
                   (list 'fletcher f)
                   (list 'miller m)
                   (list 'smith s))))
  (define (iter-b b)
    (if (= b 5)
        'done
        (iter-c b 1)))
  (define (iter-c b c)
    (cond ((= c 1) (iter-c b 2))
          ((> c 5) (iter-b (+ b 1)))
          ((= b c) (iter-c b (+ c 1)))
          (else (iter-f b c 1))))
  (define (iter-f b c f)
    (cond ((= f 1) (iter-f b c 2))
          ((= f 5) (iter-c b (+ c 1)))
          ((or (= f b) (= f c)) (iter-f b c (+ f 1)))
          ((adjacent? f c) (iter-f b c (+ f 1)))
          (else (iter-m b c f 1))))
  (define (iter-m b c f m)
    (cond ((or (= m b) (= m c) (= m f)) (iter-m b c f (+ m 1)))
          ((< m c) (iter-m b c f (+ m 1)))
          ((> m 5) (iter-f b c (+ f 1)))
          (else (iter-s b c f m 1))))
  (define (iter-s b c f m s)
    (cond ((or (= s b) (= s c) (= s f) (= s m)) (iter-s b c f m (+ s 1)))
          ((adjacent? s f) (iter-s b c f m (+ s 1)))
          ((> s 5) (iter-m b c f (+ m 1)))
          (else (display-soln b c f m s))))
  (iter-b 1))

(display "Ordinary scheme solution to the multiple-dwelling puzzle: ")
(multiple-dwelling-ordinary)
(newline)(newline)(newline)



(display "Exercise 4.42")(newline)

(define (require-one p q)
  (require (or (and p (not q)) (and (not p) q))))

(define (liars)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (require-one (= kitty 2) (= betty 3))
    (require-one (= ethel 1) (= joan 2))
    (require-one (= joan 3) (= ethel 5))
    (require-one (= kitty 2) (= mary 4))
    (require-one (= mary 4) (= betty 1))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

(display "Solution to the liars puzzle: ")
(liars)
(newline)(newline)



(display "Exercise 4.43")(newline)
(define (yachts)
  (let ((moore-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (moore-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (downing-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (downing-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (hall-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (hall-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (barnacle-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (barnacle-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (parker-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (parker-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
    (require (eq? moore-daughter 'mary-ann))
    (require (eq? moore-yacht 'lorna))
    (require
      (not (eq? moore-daughter moore-yacht)))
    (require (eq? downing-yacht 'melissa))
    (require
      (not (eq? downing-daughter downing-yacht)))
    (require (eq? hall-yacht 'rosalind))
    (require
      (not (eq? hall-daughter hall-yacht)))
    (require (eq? barnacle-yacht 'gabrielle))
    (require (eq? barnacle-daughter 'melissa))
    (require
      (not (eq? barnacle-daughter barnacle-yacht)))
    (require
      (not (eq? parker-daughter parker-yacht)))
    (require
      (distinct? (list moore-daughter downing-daughter hall-daughter barnacle-daughter parker-daughter)))
    (require
      (distinct? (list moore-yacht downing-yacht hall-yacht barnacle-yacht parker-yacht)))
    (cond ((eq? moore-daughter 'gabrielle)
           (require (eq? moore-yacht parker-daughter)))
          ((eq? downing-daughter 'gabrielle)
           (require (eq? downing-yacht parker-daughter)))
          ((eq? hall-daughter 'gabrielle)
           (require (eq? hall-yacht parker-daughter)))
          ((eq? barnacle-daughter 'gabrielle)
           (require (eq? barnacle-yacht parker-daughter))))
    (list (list 'moore
                (list 'daughter moore-daughter)
                (list 'yacht moore-yacht))
          (list 'downing
                (list 'daughter downing-daughter)
                (list 'yacht downing-yacht))
          (list 'hall
                (list 'daughter hall-daughter)
                (list 'yacht hall-yacht))
          (list 'barnacle
                (list 'daughter barnacle-daughter)
                (list 'yacht barnacle-yacht))
          (list 'parker
                (list 'daughter parker-daughter)
                (list 'yacht parker-yacht)))))

(display "Solution to the puzzle: ")
(yachts)
(newline)(newline)


(display "Exercise 4.44")(newline)
(define (queens)
  (define (safe? positions)
    (if (null? positions)
        #t
        (and (safe-single-col? (car positions) (cdr positions) 1)
             (safe? (cdr positions)))))
  (define (safe-single-col? pos1 rest-pos offset)
    (if (null? rest-pos)
        #t
        (and (not (= pos1 (car rest-pos)))
             (not (= (+ pos1 offset) (car rest-pos)))
             (not (= (- pos1 offset) (car rest-pos)))
             (safe-single-col? pos1 (cdr rest-pos) (+ offset 1)))))
  (let ((col1 (amb 1 2 3 4 5 6 7 8))
        (col2 (amb 1 2 3 4 5 6 7 8))
        (col3 (amb 1 2 3 4 5 6 7 8))
        (col4 (amb 1 2 3 4 5 6 7 8))
        (col5 (amb 1 2 3 4 5 6 7 8))
        (col6 (amb 1 2 3 4 5 6 7 8))
        (col7 (amb 1 2 3 4 5 6 7 8))
        (col8 (amb 1 2 3 4 5 6 7 8)))
    (let ((positions (list col1 col2 col3 col4 col5 col6 col7 col8)))
      (require (distinct? positions))
      (require (safe? positions))
      positions)))

(display "One solution to the queens puzzle: ")
(queens)
      