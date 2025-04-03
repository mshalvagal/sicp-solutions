#lang sicp
(#%require "register-machine.scm")

;Exercise 5.2
(newline)
(display "Exercise 5.2\n")
(display "Factorial (iterative implementation)\n")
(define iter-factorial-machine
  (make-machine
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
((recursive-expt-machine 'stack) 'initialize)
(start recursive-expt-machine)
(display "2^3 = ")
(get-register-contents recursive-expt-machine 'val)
((recursive-expt-machine 'stack) 'print-statistics)
(newline)

(set-register-contents! recursive-expt-machine 'n 10)
((recursive-expt-machine 'stack) 'initialize)
(start recursive-expt-machine)
(display "2^10 = ")
(get-register-contents recursive-expt-machine 'val)
((recursive-expt-machine 'stack) 'print-statistics)
(newline)

(newline)

;Exercise 5.4b
;Iterative expt
(display "Exercise 5.4b\n")
(display "Iterative exponentiation\n")
(define iterative-expt-machine
  (make-machine
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
((iterative-expt-machine 'stack) 'initialize)
(start iterative-expt-machine)
(display "2^3 = ")
(get-register-contents iterative-expt-machine 'val)
((iterative-expt-machine 'stack) 'print-statistics)
(newline)

(set-register-contents! iterative-expt-machine 'n 10)
((iterative-expt-machine 'stack) 'initialize)
(start iterative-expt-machine)
(display "2^10 = ")
(get-register-contents iterative-expt-machine 'val)
((iterative-expt-machine 'stack) 'print-statistics)
(newline)


(newline)(newline)

;Exercise 5.6
(display "Exercise 5.6\n")
(display "Fibonacci machine (eliminating one save and restore)\n")
(define fibonacci-machine
  (make-machine
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
((fibonacci-machine 'stack) 'initialize)
(start fibonacci-machine)
(display "Fib(3) = ")
(get-register-contents fibonacci-machine 'val)
((fibonacci-machine 'stack) 'print-statistics)
(newline)

(set-register-contents! fibonacci-machine 'n 4)
((fibonacci-machine 'stack) 'initialize)
(start fibonacci-machine)
(display "Fib(4) = ")
(get-register-contents fibonacci-machine 'val)
((fibonacci-machine 'stack) 'print-statistics)
(newline)

(set-register-contents! fibonacci-machine 'n 5)
((fibonacci-machine 'stack) 'initialize)
(start fibonacci-machine)
(display "Fib(5) = ")
(get-register-contents fibonacci-machine 'val)
((fibonacci-machine 'stack) 'print-statistics)
(newline)


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

(set-register-contents! fibonacci-machine-2 'n 3)
((fibonacci-machine-2 'stack) 'initialize)
(start fibonacci-machine-2)
(display "Fib(3) = ")
(get-register-contents fibonacci-machine-2 'val)
((fibonacci-machine-2 'stack) 'print-statistics)
(newline)

(set-register-contents! fibonacci-machine-2 'n 4)
((fibonacci-machine-2 'stack) 'initialize)
(start fibonacci-machine-2)
(display "Fib(4) = ")
(get-register-contents fibonacci-machine-2 'val)
((fibonacci-machine-2 'stack) 'print-statistics)
(newline)

(set-register-contents! fibonacci-machine-2 'n 5)
((fibonacci-machine-2 'stack) 'initialize)
(start fibonacci-machine-2)
(display "Fib(5) = ")
(get-register-contents fibonacci-machine-2 'val)
((fibonacci-machine-2 'stack) 'print-statistics)
(newline)

;TODO: 5.11b and 5.11c


(newline)(newline)


(define (compact-print-list lst)
  ; Helper to check if an item is a simple atom-only list
  (define (simple-list? item)
    (and (pair? item)
         (all-atoms? item)))
  
  ; Check if all items in a list are atoms (not pairs)
  (define (all-atoms? items)
    (cond
      ((null? items) true)
      ((pair? (car items)) false)
      (else (all-atoms? (cdr items)))))
  
  ; Create a string of spaces for indentation
  (define (make-spaces n)
    (if (<= n 0)
        ""
        (string-append " " (make-spaces (- n 1)))))
  
  ; Main recursive printing function
  (define (print-rec lst indent)
    (cond
      ((null? lst) (display "()"))
      ((not (pair? lst)) (display lst))
      ((simple-list? lst)
       ; Print lists with only atoms on a single line
       (display "(")
       (let loop ((items lst))
         (cond
           ((null? items) (display ")"))
           (else
            (display (car items))
            (if (not (null? (cdr items)))
                (display " ")
                '())
            (loop (cdr items))))))
      (else
       ; Complex list with nested structures
       (display "(")
       ; Handle the first element
       (print-rec (car lst) (+ indent 2))
       
       ; Handle the rest
       (let loop ((rest (cdr lst)))
         (cond
           ((null? rest) (display ")"))
           (else
            (if (or (not (pair? (car rest)))
                    (simple-list? (car rest)))
                (display " ") ; Simple item or simple list, stay on same line
                (begin      ; Complex nested item, new line with indent
                  (newline)
                  (display (make-spaces indent))))
            (print-rec (car rest) (+ indent 2))
            (loop (cdr rest))))))))
  
  ; Start the printing process
  (print-rec lst 0)
  (newline))


;Exercise 5.12
(display "Exercise 5.12\n")
(display "Fibonacci machine analyzer\n")
(newline)
(display "Sorted instructions:\n")
(fibonacci-machine 'get-instructions)
(newline)
(display "Entry points:\n")
(fibonacci-machine 'get-entry-points)
(newline)
(display "Stack registers:\n")
(fibonacci-machine 'get-stack-registers)
(newline)
(display "Register source lists:\n")
(compact-print-list (fibonacci-machine 'get-register-sources))


(newline)(newline)


;Exercise 5.14, 5.15
(display "Exercise 5.14, 5.15\n")
(display "Factorial (recursive implementation)\n")
(display "num_stack_pushes = 2(n -1)\n")
(define rec-factorial-machine
  (make-machine
   (list (list '* *) (list '= =) (list '- -))
   '(controller
       (perform (op initialize-stack))
       (assign continue (label fact-done))     ; set up final return address
     fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       ;; Set up for the recursive call by saving n and continue.
       ;; Set up continue so that the computation will continue
       ;; at after-fact when the subroutine returns.
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
     after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
       (goto (reg continue))                   ; return to caller
     base-case
       (assign val (const 1))                  ; base case: 1! = 1
       (goto (reg continue))                   ; return to caller
     fact-done
       (perform (op print-stack-statistics)))))

(set-register-contents! rec-factorial-machine 'n 3)
(start rec-factorial-machine)
(display "Instruction count: ")
(rec-factorial-machine 'get-exec-count)
(display "fact(3) = ")
(get-register-contents rec-factorial-machine 'val)

(set-register-contents! rec-factorial-machine 'n 4)
(start rec-factorial-machine)
(display "Instruction count: ")
(rec-factorial-machine 'get-exec-count)
(display "fact(4) = ")
(get-register-contents rec-factorial-machine 'val)

(set-register-contents! rec-factorial-machine 'n 5)
(start rec-factorial-machine)
(display "Instruction count: ")
(rec-factorial-machine 'get-exec-count)
(display "fact(5) = ")
(get-register-contents rec-factorial-machine 'val)

(set-register-contents! rec-factorial-machine 'n 6)
(start rec-factorial-machine)
(display "Instruction count: ")
(rec-factorial-machine 'get-exec-count)
(display "fact(6) = ")
(get-register-contents rec-factorial-machine 'val)

(set-register-contents! rec-factorial-machine 'n 10)
(start rec-factorial-machine)
(display "Instruction count: ")
(rec-factorial-machine 'get-exec-count)
(display "fact(10) = ")
(get-register-contents rec-factorial-machine 'val)


(newline)(newline)


;Exercise 5.16, 5.17
(display "Exercise 5.16, 5.17\n")
(display "Tracing fact(4)\n")
(rec-factorial-machine 'trace-on)
(set-register-contents! rec-factorial-machine 'n 4)
(start rec-factorial-machine)
(display "Instruction count: ")
(rec-factorial-machine 'get-exec-count)
(display "fact(4) = ")
(get-register-contents rec-factorial-machine 'val)
(rec-factorial-machine 'trace-off)


(newline)(newline)


;Exercise 5.18
(display "Exercise 5.18\n")
(display "Tracing register val during computation of fact(5)\n")
(trace-register rec-factorial-machine 'val)
(set-register-contents! rec-factorial-machine 'n 5)
(start rec-factorial-machine)
(display "fact(5) = ")
(get-register-contents rec-factorial-machine 'val)
(stop-trace-register rec-factorial-machine 'val)


(newline)(newline)


;Exercise 5.19
(display "Exercise 5.19\n")
(set-breakpoint rec-factorial-machine 'after-fact 2)
(set-breakpoint rec-factorial-machine 'after-fact 1)
(cancel-breakpoint rec-factorial-machine 'after-fact 1) ;delete breakpoint
(rec-factorial-machine 'trace-on)
(display "Breaking at 2nd line after 'after-fact during computation of fact(5)\n")
(set-register-contents! rec-factorial-machine 'n 5)
(start rec-factorial-machine)
(display "reg val = ")
(get-register-contents rec-factorial-machine 'val)
(newline)
(display "Proceeding\n")
(proceed-machine rec-factorial-machine)
(display "reg val = ")
(get-register-contents rec-factorial-machine 'val)
(cancel-all-breakpoints rec-factorial-machine) ;delete breakpoint
(newline)
(display "Proceeding (removed breakpoints)\n")
(proceed-machine rec-factorial-machine)
(display "fact(5) = ")
(get-register-contents rec-factorial-machine 'val)