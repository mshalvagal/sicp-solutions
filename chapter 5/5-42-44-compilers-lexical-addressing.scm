#lang sicp

(#%require "lexical-addressing-utils.scm")
(#%require "compiler-lexical-addressing.scm")

;Exercise 5.42
(newline)
(display "Exercise 5.42\n")
(display "Compiling lambda with lexical addressing\n")

(display "((lambda (x y)
             (lambda (a b c d e)
               ((lambda (y z) (* x y z))
                (* a b x)
                (+ c d x))))
            3
            4)\n\n")

(compile '((lambda (x y)
             (lambda (a b c d e)
               ((lambda (y z) (* x y z))
                (* a b x)
                (+ c d x))))
           3
           4)
          'val 'next the-empty-cenvironment)
(newline)(newline)


;Exercise 5.43
(newline)
(display "Exercise 5.43\n")
(display "Scanning out internal definitions in \n(define (f) (define a 1) a (define b 2) (+ a b))\n")
(compile '(define (f) (define a 1) a (define b 2) (+ a b)) 'val 'next the-empty-cenvironment)
(newline)(newline)


;Exercise 5.44
(newline)
(display "Exercise 5.44\n")
(display "Freeing up open-coded op bindings to allow parametrizing them\n")
(display "Compiling\n(lambda (+ * a b x y) (+ (* a x) (* b y)))\ndoesn't do a lexical lookup without the fix\nBut now it does.\n")
(newline)
(display "Original compiled code without fix:\n")
(display "((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry20) (reg env))
  (goto (label after-lambda21))
  entry20
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (+ * a b x y)) (reg argl) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign arg2 (op lexical-address-lookup) (const (0 4)) (reg env))
  (assign arg1 (op *) (reg arg1) (reg arg2))
  (save arg1)
  (assign arg1 (op lexical-address-lookup) (const (0 3)) (reg env))
  (assign arg2 (op lexical-address-lookup) (const (0 5)) (reg env))
  (assign arg2 (op *) (reg arg1) (reg arg2))
  (restore arg1)
  (assign val (op +) (reg arg1) (reg arg2))
  (goto (reg continue))
  after-lambda21))\n")
(newline)
(display "With fix:\n")
(compile '(lambda (+ * a b x y) (+ (* a x) (* b y))) 'val 'next the-empty-cenvironment)
(newline)(newline)