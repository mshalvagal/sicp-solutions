#lang sicp

(#%require "ecevaluator_utils.scm")

(#%provide delay-it list-of-delayed-args make-evaluated-thunk
           thunk? thunk-exp thunk-env evaluated-thunk? thunk-value)

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (make-evaluated-thunk thunk thunk-value)
  (set-car! thunk 'evaluated-thunk)
  (set-car! (cdr thunk) thunk-value)  ; replace exp with its value
  (set-cdr! (cdr thunk) '()))     ; forget unneeded env

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))