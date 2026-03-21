#lang sicp
(#%require "register-machine.scm")

;Exercise 5.2
(newline)
(display "Exercise 5.21a\n")
(display "Recursive count-leaves\n")
(define count-leaves-machine-1
  (make-machine
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?) (list 'car car) (list'cdr cdr))
   '(controller
       (assign continue (label count-done))
     count-loop
       (test (op null?) (reg tree))
       (branch (label base-case))
       (test (op pair?) (reg tree))
       (branch (label sub-trees))
       (assign count (const 1))
       (goto (reg continue))
     sub-trees
       (save continue)
       (save tree)
       (assign continue (label after-car))
       (assign tree (op car) (reg tree))
       (goto (label count-loop))
     after-car
       (restore tree)
       (assign car-count (reg count))
       (save car-count)
       (assign continue (label after-cdr))
       (assign tree (op cdr) (reg tree))
       (goto (label count-loop))
     after-cdr
       (restore car-count)
       (assign count (op +) (reg count) (reg car-count))
       (restore continue)
       (goto (reg continue))
     base-case
       (assign count (const 0))
       (goto (reg continue))
     count-done
       (perform (op print-stack-statistics)))))

(set-register-contents! count-leaves-machine-1 'tree (cons (list 1 2) (list 3 4)))
(start count-leaves-machine-1)
(display "Leaf count for (cons (list 1 2) (list 3 4)) = ")
(get-register-contents count-leaves-machine-1 'count)


(set-register-contents! count-leaves-machine-1 'tree (cons (list 1 2 (list 5 6)) (list 3 4)))
(start count-leaves-machine-1)
(display "Leaf count for (cons (list 1 2 (list 5 6)) (list 3 4)) = ")
(get-register-contents count-leaves-machine-1 'count)

(newline)(newline)

(display "Exercise 5.21b\n")
(display "Recursive count-leaves with explicit counter\n")
(define count-leaves-machine-2
  (make-machine
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?) (list 'car car) (list'cdr cdr))
   '(controller
       (assign count (const 0))
       (assign continue (label count-done))
     count-loop
       (test (op null?) (reg tree))
       (branch (label base-case))
       (test (op pair?) (reg tree))
       (branch (label sub-tree))
       (assign count (op +) (reg count) (const 1))
       (goto (reg continue))
     sub-tree
       (save continue)
       (save tree)
       (assign tree (op car) (reg tree))
       (assign continue (label after-car))
       (goto (label count-loop))
     after-car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (goto (label count-loop))
     base-case
       (goto (reg continue))
     count-done
       (perform (op print-stack-statistics)))))

(set-register-contents! count-leaves-machine-2 'tree (cons (list 1 2) (list 3 4)))
(start count-leaves-machine-2)
(display "Leaf count for (cons (list 1 2) (list 3 4)) = ")
(get-register-contents count-leaves-machine-2 'count)


(set-register-contents! count-leaves-machine-2 'tree (cons (list 1 2 (list 5 6)) (list 3 4)))
(start count-leaves-machine-2)
(display "Leaf count for (cons (list 1 2 (list 5 6)) (list 3 4)) = ")
(get-register-contents count-leaves-machine-2 'count)


(newline)(newline)

(display "Exercise 5.22a\n")
(display "List append\n")
(define list-append-machine-1
  (make-machine
   (list (list 'null? null?) (list 'cons cons) (list 'car car) (list'cdr cdr))
   '(controller
       (assign continue (label append-done))
     append-loop
       (test (op null?) (reg x))
       (branch (label base-case))
       (save continue)
       (assign continue (label after-empty-x))
       (save x)
       (assign x (op cdr) (reg x))
       (goto (label append-loop))
     after-empty-x
       (restore x)
       (assign x (op car) (reg x))
       (assign y (op cons) (reg x) (reg y))
       (restore continue)
       (goto (reg continue))
     base-case
       (goto (reg continue))
     append-done
       (perform (op print-stack-statistics)))))

(set-register-contents! list-append-machine-1 'x (list 1 2))
(set-register-contents! list-append-machine-1 'y (list 3 4))
(start list-append-machine-1)
(display "Appending (list 1 2) and (list 3 4)) to get ")(newline)
(get-register-contents list-append-machine-1 'y)


(newline)(newline)

(display "Exercise 5.22b\n")
(display "List append by splicing\n")
(define list-append-machine-2
  (make-machine
   (list (list 'null? null?) (list 'set-cdr! set-cdr!) (list 'cdr cdr))
   '(controller
       (save x)
     cdr-x-loop
       (assign temp (op cdr) (reg x))
       (test (op null?) (reg temp))
       (branch (label base-case))
       (assign x (reg temp))
       (goto (label cdr-x-loop))
     base-case
       (perform (op set-cdr!) (reg x) (reg y))
       (restore x)
     append-done
       (perform (op print-stack-statistics)))))

(set-register-contents! list-append-machine-2 'x (list 1 2))
(set-register-contents! list-append-machine-2 'y (list 3 4))
(start list-append-machine-2)
(display "Appending (list 1 2) and (list 3 4)) to get ")(newline)
(get-register-contents list-append-machine-2 'x)