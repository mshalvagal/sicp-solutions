#lang sicp

(#%require "register-machine.scm")
(#%require "ecevaluator_utils.scm")
(#%require "lexical-addressing-utils.scm")
(#%require "compiler-lexical-addressing.scm")

(#%provide compiler-machine)

;;new procedure for compiling and return the assembled instructionsc (Exercise 5.48)
(define (compile-and-assemble exp)
  (assemble (statements
             (compile exp 'val 'return the-empty-cenvironment))
            compiler-machine))

(define compiler-operations
  (list
   
   ;;primitive Scheme operations
   (list 'read read)
   
   (list 'true? true?)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'get-global-environment get-global-environment)
   
   (list 'compound-procedure? compound-procedure?)

   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'make-compiled-procedure make-compiled-procedure)
   
   (list 'lexical-address-lookup lexical-address-lookup)
   (list 'lexical-address-set! lexical-address-set!)
  
   ;; open-coded primitives (Exercise 5.38)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '= =)
   (list 'cons cons)
   (list 'list list)
   (list 'false? false?)

   ;; compilation (Exercise 5.48)
   (list 'compile-and-assemble compile-and-assemble)
   ))

(define compiler-machine
  (make-machine
   compiler-operations
   '(
     read-compile-execute-print-loop
       (perform (op initialize-stack))
       (perform
        (op prompt-for-input) (const ";;; RCEPL input:"))
       (assign exp (op read))
       (assign env (op get-global-environment))
       (assign continue (label print-result))
       (goto (label compile-and-run))
     print-result
       ;;**following instruction optional -- if use it, need monitored stack
       (perform (op print-stack-statistics))
       (perform
        (op announce-output) (const ";;; RCEPL value:"))
       (perform (op user-print) (reg val))
       (goto (label read-compile-execute-print-loop))

     compile-and-run
       (assign val (op compile-and-assemble) (reg exp))
       (assign continue (label print-result))
       (goto (reg val))
     )))

'(EXPLICIT COMPILER MACHINE LOADED)
(start compiler-machine)