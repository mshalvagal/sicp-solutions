#lang sicp

(#%require "ecevaluator_utils.scm")

(#%provide lexical-address-lookup
           lexical-address-set!
           make-lexical-address
           the-empty-cenvironment)

;; Exercise 5.39
(define (lexical-address-lookup address runtime-env)
  (define (var-loop vals i)
    (cond ((null? vals)
           (error "Unbound variable -- LOOKUP" address))
          ((= i 0)
           (let ((val (car vals)))
             (if (eq? val `*unassigned*)
                 (error "Unassigned variable value -- LOOKUP" address)
                 val)))
          (else (var-loop (cdr vals) (- i 1)))))
  (define (env-loop env n)
    (cond ((eq? env the-empty-environment)
           (error "Invalid environment -- LOOKUP" address))
          ((= n 0)
           (var-loop (frame-values (first-frame env))
                     (frame-offset address)))
          (else
           (env-loop (enclosing-environment env) (- n 1)))))
  (env-loop runtime-env (env-offset address)))

(define (lexical-address-set! address val runtime-env)
  (define (var-loop vals i)
    (cond ((null? vals)
           (error "Unbound variable -- SET!" address))
          ((= i 0)
           (set-car! vals val)
           'ok)
          (else (var-loop (cdr vals) (- i 1)))))
  (define (env-loop env n)
    (cond ((eq? env the-empty-environment)
           (error "Invalid environment -- SET!" address))
          ((= n 0)
           (var-loop (frame-values (first-frame env))
                     (frame-offset address)))
          (else
           (env-loop (enclosing-environment env) (- n 1)))))
  (env-loop runtime-env (env-offset address)))

(define (env-offset lexical-address) (car lexical-address))
(define (frame-offset lexical-address) (cadr lexical-address))

(define (make-lexical-address frame-offset var-offset)
  (list frame-offset var-offset))

(define the-empty-cenvironment '())