#lang sicp

(#%require "compiler.scm")
(#%require "compiler-l2r.scm")
(#%require "compiler-inefficient-preserve.scm")
(#%require "compiler-efficient.scm")


;Exercise 5.33
(newline)
(display "Exercise 5.33\n")
(display "Compiling alternative definition of factorial\n")

(display "(define (factorial-alt n)
            (if (= n 1)
              1
              (* n (factorial-alt (- n 1)))))\n\n")

(compile '(define (factorial-alt n)
            (if (= n 1)
              1
              (* n (factorial-alt (- n 1)))))
          'val 'next)


(newline)(newline)

;Exercise 5.34
(newline)
(display "Exercise 5.34\n")
(display "Compiling iterative definition of factorial\n")

(display "(define (factorial n)
            (define (iter product counter)
              (if (> counter n)
                  product
                  (iter (* counter product)
                        (+ counter 1))))
            (iter 1 1))\n\n")

(compile '(define (factorial n)
            (define (iter product counter)
              (if (> counter n)
                  product
                  (iter (* counter product)
                        (+ counter 1))))
            (iter 1 1))
          'val 'next)


(newline)(newline)

;Exercise 5.35
(newline)
(display "Exercise 5.35\n")
(display "Machine code to Lisp\n")

(display "(define (f x)
            (+ x (g (+ x 2))))\n\n")

(compile '(define (f x)
            (+ x (g (+ x 2))))
          'val 'next)


(newline)(newline)

;Exercise 5.36
(newline)
(display "Exercise 5.36\n")
(display "Left-to-right evaluation order\nInefficient because it relies on an append operation internally.\nO(k) per argument, so O(n^2) overall instead of O(1) in the right-to-left evaluation.\n")

(display "(define (f x)
            (+ x (g (+ x 2))))\n\n")

(compile-left-to-right '(define (f x)
                          (+ x (g (+ x 2))))
                       'val 'next)


(newline)(newline)

;Exercise 5.37
(newline)
(display "Exercise 5.37\n\n")
(display "Original preserve.\n")

(display "(+ 1 2)\n\n")
(compile '(+ 1 2) 'val 'next)
(newline)(newline)

(display "Inefficient preserve.\n")

(display "(+ 1 2)\n\n")
(compile-bad-preserve '(+ 1 2) 'val 'next)


;Exercise 5.38
(newline)
(display "Exercise 5.38\n")
(display "Compiling factorial\n")

(display "(define (factorial n)
            (if (= n 1)
              1
              (* (factorial (- n 1)) n)))\n\n")

(compile '(define (factorial n)
            (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
          'val 'next)
(newline)(newline)

(display "Compiling factorial with open-coded primitives\n")

(display "(define (factorial n)
            (if (= n 1)
              1
              (* (factorial (- n 1)) n)))\n\n")

(compile-efficient '(define (factorial n)
                      (if (= n 1)
                          1
                          (* (factorial (- n 1)) n)))
                   'val 'next)
(newline)(newline)

