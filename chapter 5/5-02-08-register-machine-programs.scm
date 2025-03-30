#lang sicp
(#%require "register-machine.scm")

;Exercise 5.2
(newline)
(display "Exercise 5.2\n")
(display "Factorial (iterative implementation)\n")
(define iter-factorial-machine
  (make-machine
   '(prod count n)
   (list (list '> >) (list '* *) (list '+ +))
   '(controller
       (assign prod (const 1))
       (assign count (const 1))
     test-count
       (test (op >) (reg count) (reg n))
       (branch (label fact-done))
       (assign prod (op *) (reg prod) (reg count))
       (assign count (op +) (reg count) (const 1))
       (goto (label test-count))
     fact-done)))

(set-register-contents! iter-factorial-machine 'n 3)
(start iter-factorial-machine)
(display "fact(3) = ")
(get-register-contents iter-factorial-machine 'prod)


(set-register-contents! iter-factorial-machine 'n 4)
(start iter-factorial-machine)
(display "fact(4) = ")
(get-register-contents iter-factorial-machine 'prod)


(newline)(newline)


;Exercise 5.3a
(display "Exercise 5.3a\n")
(display "Sqrt Newton method (abstract functions)\n")
(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))
(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define sqrt-newton-machine-1
  (make-machine
   '(guess x)
   (list (list 'good-enough? good-enough?) (list 'improve improve))
   '(controller
       (assign guess (const 1))
     test-sqrt
       (test (op good-enough?) (reg guess) (reg x))
       (branch (label sqrt-done))
       (assign guess (op improve) (reg guess) (reg x))
       (goto (label test-sqrt))
     sqrt-done)))

(set-register-contents! sqrt-newton-machine-1 'x 4)
(start sqrt-newton-machine-1)
(display "sqrt(4) = ")
(get-register-contents sqrt-newton-machine-1 'guess)

(set-register-contents! sqrt-newton-machine-1 'x 2)
(start sqrt-newton-machine-1)
(display "sqrt(2) = ")
(get-register-contents sqrt-newton-machine-1 'guess)

(newline)

;Exercise 5.3b
(display "Exercise 5.3b\n")
(display "Sqrt Newton method (primitive functions)\n")
(define sqrt-newton-machine-2
  (make-machine
   '(guess x t)
   (list (list '* *) (list '+ +) (list '- -) (list '/ /) (list '< <) (list 'abs abs))
   '(controller
       (assign guess (const 1))
     test-sqrt
       (assign t (op *) (reg guess) (reg guess))
       (assign t (op -) (reg t) (reg x))
       (assign t (op abs) (reg t))
       (test (op <) (reg t) (const 0.001))
       (branch (label sqrt-done))
       (assign t (op /) (reg x) (reg guess))
       (assign guess (op +) (reg t) (reg guess))
       (assign guess (op /) (reg guess) (const 2))
       (goto (label test-sqrt))
     sqrt-done)))

(set-register-contents! sqrt-newton-machine-2 'x 4)
(start sqrt-newton-machine-2)
(display "sqrt(4) = ")
(get-register-contents sqrt-newton-machine-2 'guess)

(set-register-contents! sqrt-newton-machine-2 'x 2)
(start sqrt-newton-machine-2)
(display "sqrt(2) = ")
(get-register-contents sqrt-newton-machine-2 'guess)


(newline)(newline)


;Exercise 5.4a
;Recursive expt
(display "Exercise 5.4a\n")
(display "Recursive exponentiation\n")
(define recursive-expt-machine
  (make-machine
   '(n val b continue)
   (list (list '* *) (list '= =) (list '- -))
   '(controller
       (assign continue (label expt-done))
     expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save continue)
       (assign continue (label after-loop))
       (assign n (op -) (reg n) (const 1))
       (goto (label expt-loop))
     after-loop
       (restore continue)
       (assign val (op *) (reg val) (reg b))
       (goto (reg continue))
     base-case
       (assign val (const 1))
       (goto (reg continue))
     expt-done)))

(set-register-contents! recursive-expt-machine 'b 2)
(set-register-contents! recursive-expt-machine 'n 3)
(start recursive-expt-machine)
(display "2^3 = ")
(get-register-contents recursive-expt-machine 'val)

(set-register-contents! recursive-expt-machine 'n 10)
(start recursive-expt-machine)
(display "2^10 = ")
(get-register-contents recursive-expt-machine 'val)

(newline)

;Exercise 5.4b
;Iterative expt
(display "Exercise 5.4b\n")
(display "Iterative exponentiation\n")
(define iterative-expt-machine
  (make-machine
   '(n val b)
   (list (list '* *) (list '= =) (list '- -))
   '(controller
       (assign val (const 1))
     expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label expt-done))
       (assign val (op *) (reg val) (reg b))
       (assign n (op -) (reg n) (const 1))
       (goto (label expt-loop))
     expt-done)))

(set-register-contents! iterative-expt-machine 'b 2)
(set-register-contents! iterative-expt-machine 'n 3)
(start iterative-expt-machine)
(display "2^3 = ")
(get-register-contents iterative-expt-machine 'val)

(set-register-contents! iterative-expt-machine 'n 10)
(start iterative-expt-machine)
(display "2^10 = ")
(get-register-contents iterative-expt-machine 'val)


(newline)(newline)

;Exercise 5.6
(display "Exercise 5.6\n")
(display "Fibonacci machine (eliminating one save and restore)\n")
(define fibonacci-machine
  (make-machine
   '(continue n val)
   (list (list '+ +) (list '- -) (list '< <))
   '(controller
       (assign continue (label fib-done))
       fib-loop
         (test (op <) (reg n) (const 2))
         (branch (label immediate-answer))
         ;; set up to compute Fib(n - 1)
         (save continue)
         (assign continue (label afterfib-n-1))
         (save n)                           ; save old value of n
         (assign n (op -) (reg n) (const 1)); clobber n to n - 1
         (goto (label fib-loop))            ; perform recursive call
       afterfib-n-1                         ; upon return, val contains Fib(n - 1)
         (restore n)
         ;; set up to compute Fib(n - 2)
         (assign n (op -) (reg n) (const 2))
         (assign continue (label afterfib-n-2))
         (save val)                         ; save Fib(n - 1)
         (goto (label fib-loop))
       afterfib-n-2                         ; upon return, val contains Fib(n - 2)
         (assign n (reg val))               ; n now contains Fib(n - 2)
         (restore val)                      ; val now contains Fib(n - 1)
         (restore continue)
         (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
                 (op +) (reg val) (reg n)) 
         (goto (reg continue))              ; return to caller, answer is in val
       immediate-answer
         (assign val (reg n))               ; base case:  Fib(n) = n
         (goto (reg continue))
       fib-done)))

(set-register-contents! fibonacci-machine 'n 3)
(start fibonacci-machine)
(display "Fib(3) = ")
(get-register-contents fibonacci-machine 'val)

(set-register-contents! fibonacci-machine 'n 4)
(start fibonacci-machine)
(display "Fib(4) = ")
(get-register-contents fibonacci-machine 'val)

(set-register-contents! fibonacci-machine 'n 5)
(start fibonacci-machine)
(display "Fib(5) = ")
(get-register-contents fibonacci-machine 'val)


(newline)(newline)

#|
;Exercise 5.8
(display "Exercise 5.8\n")
(display "Should throw an error\n")
(define repeat-label-machine
  (make-machine
   '(a)
   (list (list '+ +))
   '(start
       (goto (label here))
     here
       (assign a (const 3))
       (goto (label there))
     here
       (assign a (const 4))
       (goto (label there))
     there)))

(start repeat-label-machine)
(get-register-contents repeat-label-machine 'a)
|#



;Exercise 5.11a
(display "Exercise 5.11a\n")
(display "Fibonacci machine exploiting stack behavior\n")
(define fibonacci-machine-2
  (make-machine
   '(continue n val)
   (list (list '+ +) (list '- -) (list '< <))
   '(controller
       (assign continue (label fib-done))
       fib-loop
         (test (op <) (reg n) (const 2))
         (branch (label immediate-answer))
         ;; set up to compute Fib(n - 1)
         (save continue)
         (assign continue (label afterfib-n-1))
         (save n)                           ; save old value of n
         (assign n (op -) (reg n) (const 1)); clobber n to n - 1
         (goto (label fib-loop))            ; perform recursive call
       afterfib-n-1                         ; upon return, val contains Fib(n - 1)
         (restore n)
         ;; set up to compute Fib(n - 2)
         (assign n (op -) (reg n) (const 2))
         (assign continue (label afterfib-n-2))
         (save val)                         ; save Fib(n - 1)
         (goto (label fib-loop))
       afterfib-n-2                         ; upon return, val contains Fib(n - 2)
         (restore n)                        ; n now contains Fib(n - 1)
         (restore continue)
         (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
                 (op +) (reg val) (reg n)) 
         (goto (reg continue))              ; return to caller, answer is in val
       immediate-answer
         (assign val (reg n))               ; base case:  Fib(n) = n
         (goto (reg continue))
       fib-done)))

(set-register-contents! fibonacci-machine 'n 3)
(start fibonacci-machine)
(display "Fib(3) = ")
(get-register-contents fibonacci-machine 'val)

(set-register-contents! fibonacci-machine 'n 4)
(start fibonacci-machine)
(display "Fib(4) = ")
(get-register-contents fibonacci-machine 'val)

(set-register-contents! fibonacci-machine 'n 5)
(start fibonacci-machine)
(display "Fib(5) = ")
(get-register-contents fibonacci-machine 'val)

;TODO: 5.11b and 5.11c

