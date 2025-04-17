#lang sicp
(#%require "register-machine.scm")

;Exercise 5.2
(newline)
(display "Exercise 5.21a\n")
(display "Count-leaves (recursive implementation)\n")
(define count-leaves-machine
  (make-machine
   (list (list 'null? null?) (list 'pair? pair?) (list 'car car) (list 'cdr cdr) (list '+ +))
   '(controller
       (perform (op initialize-stack))
       (assign continue (label count-done))     ; set up final return address
     count-tests
       (test (op null?) (reg tree))
       (branch (label base-case-null))
       (test (op pair?) (reg tree))
       (branch (label count-loop))
       (goto (label base-case-leaf))
     count-loop
       (save continue)
       (save tree)
       (assign tree (op car) (reg tree))
       (assign continue (label after-count-car))
       (goto (label count-tests))
     after-count-car                 ; after return val contains count-leaves of car
       (restore tree)
       (save val)
       (assign tree (op cdr) (reg tree))
       (assign continue (label after-count-cdr))
       (goto (label count-tests))
     after-count-cdr                 ; after return val contains count-leaves of cdr, stack contains count-leaves of car
       (assign temp (reg val))
       (restore val)
       (assign val (op +) (reg val) (reg temp))
       (restore continue)
       (goto (reg continue))
     base-case-null
       (assign val (const 0))
       (goto (reg continue))
     base-case-leaf
       (assign val (const 1))
       (goto (reg continue))
     count-done)))

(set-register-contents! count-leaves-machine 'tree (list (list 1 (list 2 3)) (list 4 5) 6))
(start count-leaves-machine)
(display "count-leaves ((1 (2 3)) (4 5) 6) = ")
(get-register-contents count-leaves-machine 'val)


(newline)(newline)

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

(display "Exercise 5.21b\n")
(display "Count-leaves (iterative implementation)\n")
(define count-leaves-machine-2
  (make-machine
   (list (list 'null? null?) (list 'pair? pair?) (list 'car car) (list 'cdr cdr) (list '+ +))
   '(controller
       (perform (op initialize-stack))
       (assign continue (label count-done))     ; set up final return address
       (assign count (const 0))
     count-tests
       (test (op null?) (reg tree))
       (branch (label base-case-null))
       (test (op pair?) (reg tree))
       (branch (label count-loop))
       (assign count (op +) (reg count) (const 1))
       (goto (reg continue))
     count-loop
       (save continue)
       (save tree)
       (assign tree (op car) (reg tree))
       (assign continue (label after-count-car))
       (goto (label count-tests))
     after-count-car                 ; after return count contains count-leaves of car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (goto (label count-tests))
     base-case-null
       (goto (reg continue))
     count-done)))

(set-register-contents! count-leaves-machine-2 'tree (list (list 1 (list 2 3)) (list 4 5) 6))
(start count-leaves-machine-2)
(display "count-leaves ((1 (2 3)) (4 5) 6) = ")
(get-register-contents count-leaves-machine-2 'count)

(newline)(newline)

(display "Exercise 5.22a\n")
(display "append\n")
(define append-machine
  (make-machine
   (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'cons cons))
   '(controller
       (perform (op initialize-stack))
       (assign continue (label append-done))     ; set up final return address
     loop-first-list
       (test (op null?) (reg x))
       (branch (label base-case))
       (save continue)
       (save x)
       (assign x (op cdr) (reg x))
       (assign continue (label first-list-done))
       (goto (label loop-first-list))
     first-list-done ; x is null at the first return here
       (restore x)   ; x points to the last pair at the first return here
       (restore continue)
       (assign temp (op car) (reg x))
       (assign out-list (op cons) (reg temp) (reg out-list))
       (goto (reg continue))
     base-case                 ; x is null
       (assign out-list (reg y))
       (goto (reg continue))
     append-done)))

(set-register-contents! append-machine 'x (list 1 2))
(set-register-contents! append-machine 'y (list 3 4))
(start append-machine)
(display "append (1 2) (3 4) = ")
(get-register-contents append-machine 'out-list)

(newline)(newline)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(display "Exercise 5.22b\n")
(display "append!\n")
(define append!-machine
  (make-machine
   (list (list 'null? null?) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
   '(controller
       (perform (op initialize-stack))
       (assign continue (label append-done))     ; set up final return address
       (save x)
     loop-first-list
       (assign temp (op cdr) (reg x))
       (test (op null?) (reg temp))
       (branch (label base-case))
       (assign x (op cdr) (reg x))
       (goto (label loop-first-list))
     base-case                 ; x is null
       (perform (op set-cdr!) (reg x) (reg y))
       (restore x)
     append-done)))

(set-register-contents! append!-machine 'x (list 'a 'b))
(set-register-contents! append!-machine 'y (list 'c 'd))
(start append!-machine)
(display "append! (a b) (c d) = ")
(get-register-contents append!-machine 'x)