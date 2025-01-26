#lang sicp
(#%require "meta-lisp-evaluator-non-deterministic.scm")

(define (require p)
  (if (not p) (amb)))

(let ((x (amb 1 2 3)))
    (require (>= x 2))
  x)

(newline)
(display "Exercise 4.35")(newline)

(display "A pythagorean triple between 4 and 12: ")
(newline)
(ambtest '(begin
            (define (an-integer-starting-from n)
              (amb n (call an-integer-starting-from (call + n 1))))
            (define (an-integer-between low high)
              (call require (call <= low high))
              (amb low (call an-integer-between (call + low 1) high)))

            (define (a-pythagorean-triple-between low high)
              (let ((i (call an-integer-between low high)))
                (let ((j (call an-integer-between i high)))
                  (let ((k (call an-integer-between j high)))
                    (call require (call = (call + (call * i i) (call * j j)) (call * k k)))
                    (call list i j k)))))
            (call a-pythagorean-triple-between 4 12))
         #f
         0)

(newline)(newline)


(display "Exercise 4.36")(newline)

(display "First 10 pythagorean triples: ")
(ambtest '(begin
            (define (pythagorean-triples)
              (let ((k (call an-integer-starting-from 2)))
                (let ((i (call an-integer-between 1 k)))
                  (let ((j (call an-integer-between i k)))
                    (call require (call = (call + (call * i i) (call * j j)) (call * k k)))
                    (call list i j k)))))
            (call pythagorean-triples))
         #t
         10)

(newline)(newline)



(display "Solution to the multiple-dwelling puzzle: ")
(ambtest '(begin
            (define (distinct? items)
              (cond ((call null? items) true)
                    ((call null? (call cdr items)) true)
                    ((call member (call car items) (call cdr items)) false)
                    (else (call distinct? (call cdr items)))))
            (define (multiple-dwelling)
              (let ((baker (amb 1 2 3 4 5))
                    (cooper (amb 1 2 3 4 5))
                    (fletcher (amb 1 2 3 4 5))
                    (miller (amb 1 2 3 4 5))
                    (smith (amb 1 2 3 4 5)))
                (call require
                  (call distinct? (call list baker cooper fletcher miller smith)))
                (call require (call not (call = baker 5)))
                (call require (call not (call = cooper 1)))
                (call require (call not (call = fletcher 5)))
                (call require (call not (call = fletcher 1)))
                (call require (call > miller cooper))
                (call require (call not (call = (call abs (call - smith fletcher)) 1)))
                (call require (call not (call = (call abs (call - fletcher cooper)) 1)))
                (call list (call list 'baker baker)
                      (call list 'cooper cooper)
                      (call list 'fletcher fletcher)
                      (call list 'miller miller)
                      (call list 'smith smith))))

            (call multiple-dwelling))
         #f
         0)
(newline)(newline)



(display "Exercise 4.38")(newline)
(display "All solutions to the multiple-dwelling puzzle with the Smith Fletcher restriction removed:")
(ambtest '(begin
            (define (distinct? items)
              (cond ((call null? items) true)
                    ((call null? (call cdr items)) true)
                    ((call member (call car items) (call cdr items)) false)
                    (else (call distinct? (call cdr items)))))
            (define (multiple-dwelling)
              (let ((baker (amb 1 2 3 4 5))
                    (cooper (amb 1 2 3 4 5))
                    (fletcher (amb 1 2 3 4 5))
                    (miller (amb 1 2 3 4 5))
                    (smith (amb 1 2 3 4 5)))
                (call require
                  (call distinct? (call list baker cooper fletcher miller smith)))
                (call require (call not (call = baker 5)))
                (call require (call not (call = cooper 1)))
                (call require (call not (call = fletcher 5)))
                (call require (call not (call = fletcher 1)))
                (call require (call > miller cooper))
                (call require (call not (call = (call abs (call - fletcher cooper)) 1)))
                (call list (call list 'baker baker)
                      (call list 'cooper cooper)
                      (call list 'fletcher fletcher)
                      (call list 'miller miller)
                      (call list 'smith smith))))

            (call multiple-dwelling))
         #t
         100)
(newline)(newline)(newline)



(display "Exercise 4.39")(newline)

(display "More optimized solution (reordered restrictions) to the multiple-dwelling puzzle: ")
(ambtest '(begin
            (define (multiple-dwelling-reordered-restrictions)
              (let ((baker (amb 1 2 3 4 5))
                    (cooper (amb 1 2 3 4 5))
                    (fletcher (amb 1 2 3 4 5))
                    (miller (amb 1 2 3 4 5))
                    (smith (amb 1 2 3 4 5)))
                (call require
                  (call distinct? (call list baker cooper fletcher miller smith)))
                (call require (call not (call = (call abs (call - smith fletcher)) 1)))
                (call require (call not (call = cooper 1)))
                (call require (call > miller cooper))
                (call require (call not (call = fletcher 5)))
                (call require (call not (call = fletcher 1)))
                (call require (call not (call = (call abs (call - fletcher cooper)) 1)))
                (call require (call not (call = baker 5)))
                (call list (call list 'baker baker)
                      (call list 'cooper cooper)
                      (call list 'fletcher fletcher)
                      (call list 'miller miller)
                      (call list 'smith smith))))
            (call multiple-dwelling-reordered-restrictions))
         #f
         0)
(newline)
(display "It is in general better to put the conditions corresponding to the last branching variables first so that failures can more quickly backtrack to the relevant level.")(newline)(newline)
(display "Note that the question says we can only change the order of the requires, not of the ambiguous variable let-definitions. This is what we do next.")
(newline)(newline)


(display "Exercise 4.40")(newline)
(display "Fully optimized solution (interleaved branching and filtering) to the multiple-dwelling puzzle: ")
(ambtest '(begin
            (define (multiple-dwelling-optimized)
              (let ((baker (amb 1 2 3 4 5)))
                (call require (call not (call = baker 5)))
                (let ((cooper (amb 1 2 3 4 5)))
                  (call require (call not (call = cooper 1)))
                  (let ((fletcher (amb 1 2 3 4 5)))
                    (call require (call not (call = fletcher 1)))
                    (call require (call not (call = fletcher 5)))
                    (call require (call not (call = (call abs (call - fletcher cooper)) 1)))
                    (let ((miller (amb 1 2 3 4 5)))
                      (call require (call > miller cooper))
                      (let ((smith (amb 1 2 3 4 5)))
                        (call require (call not (call = (call abs (call - smith fletcher)) 1)))
                        (call require
                          (call distinct? (call list baker cooper fletcher miller smith)))
                        (call list (call list 'baker baker)
                              (call list 'cooper cooper)
                              (call list 'fletcher fletcher)
                              (call list 'miller miller)
                              (call list 'smith smith))))))))
            (call multiple-dwelling-optimized))
         #f
         0)

(newline)(newline)(newline)


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
(display "Solution to the liars puzzle: ")

(ambtest '(begin
            (define (require-one p q)
              (call require (or (and p (call not q)) (and (call not p) q))))

            (define (liars)
              (let ((betty (amb 1 2 3 4 5))
                    (ethel (amb 1 2 3 4 5))
                    (joan (amb 1 2 3 4 5))
                    (kitty (amb 1 2 3 4 5))
                    (mary (amb 1 2 3 4 5)))
                (call require
                  (call distinct? (call list betty ethel joan kitty mary)))
                (call require-one (call = kitty 2) (call = betty 3))
                (call require-one (call = ethel 1) (call = joan 2))
                (call require-one (call = joan 3) (call = ethel 5))
                (call require-one (call = kitty 2) (call = mary 4))
                (call require-one (call = mary 4) (call = betty 1))
                (call list (call list 'betty betty)
                      (call list 'ethel ethel)
                      (call list 'joan joan)
                      (call list 'kitty kitty)
                      (call list 'mary mary))))

            (call liars))
         #f
         0)
(newline)(newline)(newline)



(display "Exercise 4.43")(newline)

#|
(display "Solution to the yachts puzzle: ")
(ambtest '(begin
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
                (call require (call eq? moore-daughter 'mary-ann))
                (call require (call eq? moore-yacht 'lorna))
                (call require
                  (call not (call eq? moore-daughter moore-yacht)))
                (call require (call eq? downing-yacht 'melissa))
                (call require
                  (call not (call eq? downing-daughter downing-yacht)))
                (call require (call eq? hall-yacht 'rosalind))
                (call require
                  (call not (call eq? hall-daughter hall-yacht)))
                (call require (call eq? barnacle-yacht 'gabrielle))
                (call require (call eq? barnacle-daughter 'melissa))
                (call require
                  (call not (call eq? barnacle-daughter barnacle-yacht)))
                (call require
                  (call not (call eq? parker-daughter parker-yacht)))
                (call require
                  (call distinct? (call list moore-daughter downing-daughter hall-daughter barnacle-daughter parker-daughter)))
                (call require
                  (call distinct? (call list moore-yacht downing-yacht hall-yacht barnacle-yacht parker-yacht)))
                (cond ((call eq? moore-daughter 'gabrielle)
                       (call require (call eq? moore-yacht parker-daughter)))
                      ((call eq? downing-daughter 'gabrielle)
                       (call require (call eq? downing-yacht parker-daughter)))
                      ((call eq? hall-daughter 'gabrielle)
                       (call require (call eq? hall-yacht parker-daughter)))
                      ((call eq? barnacle-daughter 'gabrielle)
                       (call require (call eq? barnacle-yacht parker-daughter))))
                (call list (call list 'moore
                                 (call list 'daughter moore-daughter)
                                 (call list 'yacht moore-yacht))
                      (call list 'downing
                            (call list 'daughter downing-daughter)
                            (call list 'yacht downing-yacht))
                      (call list 'hall
                            (call list 'daughter hall-daughter)
                            (call list 'yacht hall-yacht))
                      (call list 'barnacle
                            (call list 'daughter barnacle-daughter)
                            (call list 'yacht barnacle-yacht))
                      (call list 'parker
                            (call list 'daughter parker-daughter)
                            (call list 'yacht parker-yacht)))))
            (call yachts))
         #f
         0)
(newline)(newline)
|#

(display "Fast solution to the yachts puzzle:")
(ambtest '(begin
            (define (yachts-fast mary-ann-moore)
              (let ((moore-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                (if mary-ann-moore
                    (call require (call eq? moore-daughter 'mary-ann)))
                (let ((moore-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                  (call require (call eq? moore-yacht 'lorna))
                  (call require
                        (call not (call eq? moore-daughter moore-yacht)))
                  (let ((downing-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                    (call require (call eq? downing-yacht 'melissa))
                    (let ((downing-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                      (call require
                            (call not (call eq? downing-daughter downing-yacht)))
                      (let ((hall-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                        (call require (call eq? hall-yacht 'rosalind))
                        (let ((hall-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                          (call require
                                (call not (call eq? hall-daughter hall-yacht)))
                          (let ((barnacle-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                            (call require (call eq? barnacle-yacht 'gabrielle))
                            (let ((barnacle-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                              (call require (call eq? barnacle-daughter 'melissa))
                              (call require
                                    (call not (call eq? barnacle-daughter barnacle-yacht)))
                              (let ((parker-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
                                    (parker-yacht (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
                                (call require
                                      (call not (call eq? parker-daughter parker-yacht)))
                                (call require
                                      (call distinct? (call list moore-daughter downing-daughter hall-daughter barnacle-daughter parker-daughter)))
                                (call require
                                      (call distinct? (call list moore-yacht downing-yacht hall-yacht barnacle-yacht parker-yacht)))
                                (call require (call not (call eq? parker-daughter 'gabrielle)))
                                (cond ((call eq? moore-daughter 'gabrielle)
                                       (call require (call eq? moore-yacht parker-daughter)))
                                      ((call eq? downing-daughter 'gabrielle)
                                       (call require (call eq? downing-yacht parker-daughter)))
                                      ((call eq? hall-daughter 'gabrielle)
                                       (call require (call eq? hall-yacht parker-daughter)))
                                      ((call eq? barnacle-daughter 'gabrielle)
                                       (call require (call eq? barnacle-yacht parker-daughter))))
                                (call list (call list 'moore
                                                 (call list 'daughter moore-daughter)
                                                 (call list 'yacht moore-yacht))
                                      (call list 'downing
                                            (call list 'daughter downing-daughter)
                                            (call list 'yacht downing-yacht))
                                      (call list 'hall
                                            (call list 'daughter hall-daughter)
                                            (call list 'yacht hall-yacht))
                                      (call list 'barnacle
                                            (call list 'daughter barnacle-daughter)
                                            (call list 'yacht barnacle-yacht))
                                      (call list 'parker
                                            (call list 'daughter parker-daughter)
                                            (call list 'yacht parker-yacht)))))))))))))
            (call yachts-fast true))
         #t
         5)
(newline)(newline)

(display "Solutions if Mary Ann might not be Moore:")
(ambtest '(call yachts-fast false)
         #t
         20)
(newline)(newline)(newline)


(display "Exercise 4.44")(newline)
(display "First 2 solutions to the queens puzzle (this can take a minute): ")
(ambtest '(begin
            (define (queens)
              (define (safe? positions)
                (if (call null? positions)
                    true
                    (and (call safe-single-col? (call car positions) (call cdr positions) 1)
                         (call safe? (call cdr positions)))))
              (define (safe-single-col? pos1 rest-pos offset)
                (if (call null? rest-pos)
                    true
                    (and (call not (call = pos1 (call car rest-pos)))
                         (call not (call = (call + pos1 offset) (call car rest-pos)))
                         (call not (call = (call - pos1 offset) (call car rest-pos)))
                         (call safe-single-col? pos1 (call cdr rest-pos) (call + offset 1)))))
              (let ((col1 (amb 1 2 3 4 5 6 7 8))
                    (col2 (amb 1 2 3 4 5 6 7 8))
                    (col3 (amb 1 2 3 4 5 6 7 8))
                    (col4 (amb 1 2 3 4 5 6 7 8))
                    (col5 (amb 1 2 3 4 5 6 7 8))
                    (col6 (amb 1 2 3 4 5 6 7 8))
                    (col7 (amb 1 2 3 4 5 6 7 8))
                    (col8 (amb 1 2 3 4 5 6 7 8)))
                (let ((positions (call list col1 col2 col3 col4 col5 col6 col7 col8)))
                  (call require (call distinct? positions))
                  (call require (call safe? positions))
                  positions)))
            (call queens))
         #t
         2)