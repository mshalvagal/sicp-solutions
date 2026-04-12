#lang sicp

(#%require "register-machine.scm")
(#%require "ecevaluator_utils.scm")
(#%require "ecevaluator.scm")
(#%require "compiler-lexical-addressing.scm")
(#%require "lexical-addressing-utils.scm")

(define (start-eceval)
  (reset-global-environment!)
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return the-empty-cenvironment))
                   eceval)))
    (reset-global-environment!)
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

;;; Exercise 5.45
(newline)
(display "Exercise 5.45\n\n")
(display "Interpreted factorial\nmax-depth=5n+3\npushes=32n-16\n\n")
(display "Specialized register machine\nmax-depth=2(n-1)\npushes=2(n-1)\n\n")
(display "Compiled factorial\nmax-depth=2(n-1)\npushes=2n+3\n\n")
(newline) (newline)

;;; Exercise 5.46
(newline)
(display "Exercise 5.46\n\n")
(display "Interpreted fibonacci\nmax-depth=5n+3\npushes=56fib(n+1)-40\n\n")
(display "Specialized register machine\nmax-depth=2(n-1)\npushes=3fib(n+1)-3\n\n")
(display "Compiled fibonacci\nmax-depth=2n+3\npushes=7fib(n+1)\n\n")
(newline) (newline)

;;; Exercise 5.47
(newline)
(display "Exercise 5.47\n\n")
(display "Compiling (define (f x) (+ (g x) 1)\n")
(newline) (newline)

(compile-and-go
 '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
    (define (fib n)
      (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
    (define (f x)
      (+ (g x) 1))))

(newline)(newline)