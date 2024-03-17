#lang sicp
(#%require "meta-lisp-evaluator.scm")

;Exercise 4.1
(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((first-val (eval (first-operand exps) env)))
        (cons first-val
              (list-of-values-l2r
               (rest-operands exps)
               env)))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-vals (list-of-values-r2l (rest-operands exps)
                                           env)))
        (cons (eval (first-operand exps) env)
              rest-vals))))


;Exercise 4.2
(define (application?-alt exp) (tagged-list? exp 'call))
(define (operator-alt exp) (cadr exp))
(define (operands-alt exp) (cddr exp))

;Exercise 4.3 ------------- TODO ---------------
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define (install-syntax)
  (put 'quote text-of-quotation)
  (put 'set eval-assignment)
  (put 'define eval-definition)
  (put 'if eval-if)
  (put 'lambda make-procedure)
  (put 'begin eval-sequence)
  (put 'cond cond->if)
  (put 'call apply))
(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))