#lang sicp

(#%require "meta-lisp-evaluator-normal-order.scm")

(define (try a b)
  (if (= a 0) 1 b))
; the next commented line throws an error because scheme is an applicative-order language
; (try 0 (/ 1 0))

(newline)(newline)
(display "Exercise 4.26")(newline)

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-cond exp) (cadr exp))
(define (unless-usual-val exp) (caddr exp))
(define (unless-exception-val exp) (cadddr exp))
(define (unless->if exp)
  (make-if (unless-cond exp)
           (unless-exception-val exp)
           (unless-usual-val exp)))

(put 'eval 'unless
     (lambda (exp env)
       (metacircular-eval (unless->if exp) env)))

(display "Converting to if expression and evaluating with (define b 0) (define a 1)")(newline)
(newline)
  
(display "(unless (call = b 0) (call / a b) -42)")(newline)
(display "If expression: ")(unless->if '(unless (call = b 0) (call / a b) -42))
(display "Result: ")
(metacircular-eval (sequence->exp '((define a 1)
                                    (define b 0)
                                    (unless (call = b 0) (call / a b) -42)))
                   the-global-environment)



(newline)(newline)
(display "Exercise 4.27")(newline)

(display "Evaluating the provided expressions (define count 0)
                                    (define (id x) (set! count (call + count 1)) x)
                                    (define w (call id (call id 10)))")(newline)
(display "count: ")
(metacircular-eval (sequence->exp '((define count 0)
                                    (define (id x) (set! count (call + count 1)) x)
                                    (define w (call id (call id 10)))
                                    count))
                   the-global-environment)

(display "w (unforced): ")
(metacircular-eval 'w the-global-environment)

(display "count: ")
(metacircular-eval 'count the-global-environment)


(display "w (forced): ")
(metacircular-eval (actual-value 'w the-global-environment) the-global-environment)

(display "count: ")
(metacircular-eval 'count the-global-environment)

(display "Forcing w causes the inner thunk (call id 10) to be evaluated and thus increments count")(newline)



(newline)(newline)
(display "Exercise 4.28")(newline)(newline)
(display "Operators need to be forced so that in case they are provided as arguments to other procedures, they are not still thunks but already-evaluated procedures.")
(display "For example,
(define (f x) x)
(define (g f x) (call f x))
(call g f 1)
would throw an error otherwise, because it would attempt to apply a thunk (neither a primitive nor a compound procedure) as a procedure")(newline)
(display "Currently however, it works correctly. Result: ")
(metacircular-eval (actual-value (sequence->exp '((define (f x) x)
                                                  (define (g f x) (call f x))
                                                  (call g f 1)))
                                 the-global-environment)
                   the-global-environment)



(newline)(newline)
(display "Exercise 4.29")(newline)(newline)
(display "The recursive Fibonacci function would be slower without memoization")(newline)
(display "With memoization (square (id 10)) :")
(metacircular-eval (actual-value (sequence->exp '((define (square x) (call * x x))
                                                  (define count 0)
                                                  (call square (call id 10))))
                                 the-global-environment)
                   the-global-environment)
(display "count: ")
(metacircular-eval 'count the-global-environment)
(display "Without memoization the count would be 2 instead, because the thunk (call id 10) would be forced twice independently.")(newline)
