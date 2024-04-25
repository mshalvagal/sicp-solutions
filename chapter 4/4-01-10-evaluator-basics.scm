#lang sicp
;(#%require "meta-lisp-evaluator.scm")
(#%require "meta-lisp-evaluator-data-directed.scm")
(#%provide install-syntax)
(#%provide operator)
(#%provide operands)
(#%provide make-application)
(#%provide make-definition)
(#%provide and->if)
(#%provide or->if)
(#%provide make-let)
(#%provide let->combination)
(#%provide let*->nested-lets)

;Exercise 4.1
(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((first-val (metacircular-eval (first-operand exps) env)))
        (cons first-val
              (list-of-values-l2r
               (rest-operands exps)
               env)))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-vals (list-of-values-r2l (rest-operands exps) env)))
        (cons (metacircular-eval (first-operand exps) env)
              rest-vals))))


;Exercise 4.2
(define (application?-alt exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (make-application procedure args)
  (cons 'call (cons procedure args)))

;Exercise 4.3
(display "Exercise 4.3")(newline)

(define (metacircular-eval-data-directed exp env)
  (define (exp-type exp) (car exp))
  ;(display exp)(newline)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((and (pair? exp) (symbol? (car exp)))
         (let ((op (get 'eval (exp-type exp))))
           (if op
               (op exp env)
               (error "Operation not found -- EVAL" exp))))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (install-syntax)
  (put 'eval 'quote
       (lambda (exp env)
         (text-of-quotation exp)))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda
       (lambda (exp env)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)))
  (put 'eval 'begin
       (lambda (exp env)
         (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond
       (lambda (exp env)
         (metacircular-eval (cond->if exp) env)))
  (put 'eval 'call
       (lambda (exp env)
         ;(display exp)(newline)
         ;(display "Operator: ")(display (metacircular-eval (operator exp) env))(newline)
         ;(display "Operands: ")(display (operands exp))(newline)
         ;(display "Operands: ")(display (metacircular-eval (list-of-values (operands exp) env) env))(newline)
         (metacircular-apply (metacircular-eval (operator exp) env)
                             (list-of-values (operands exp) env))))
  'ok)

(display "Installing the data-directed eval syntax")(newline)
(install-syntax)
(display "Testing (call * 2 2)")(newline)
(display "Result: ")(metacircular-eval '(call * 2 2) the-global-environment)
(display "Testing (call (lambda (x y) (+ (* x x) y)) 3 2)")(newline)
(metacircular-eval '(call (lambda (x y) (call + (call * x x) y)) 3 2) the-global-environment)

;Exercise 4.4
(newline)
(display "Exercise 4.4")(newline)
;special forms
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and-clauses exp) (cdr exp))
(define (or-clauses exp) (cdr exp))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (last-clause? clauses) (null? (cdr clauses)))
(define (eval-and clauses env)
  (if (null? clauses)
      'true
      (let ((first-clause-result (metacircular-eval (first-clause clauses) env)))
        (if (true? first-clause-result)
            (if (last-clause? clauses)
                first-clause-result
                (eval-and (rest-clauses clauses) env))
            'false))))
(define (eval-or clauses env)
  (if (null? clauses)
      'false
      (let ((first-clause-result (metacircular-eval (first-clause clauses) env)))
        (if (true? first-clause-result)
            first-clause-result
            (eval-or (rest-clauses clauses) env)))))

(display "Installing eval-and and eval-or as special forms")(newline)
(put 'eval 'and
     (lambda (exp env)
       (eval-and (and-clauses exp) env)))
(put 'eval 'or
     (lambda (exp env)
       (eval-or (or-clauses exp) env)))

(display "Testing evaluation")(newline)

(display "(and 1 2)")(newline)
(metacircular-eval '(and 1 2) the-global-environment) ;; 2
;(metacircular-eval '(call and 1 2) the-global-environment)

(display "(and false 2)")(newline)
(metacircular-eval '(and false 2) the-global-environment) ;; false

(display "(or 1 2)")(newline)
(metacircular-eval '(or 1 2) the-global-environment) ;; 1

(display "(or false 2)")(newline)
(metacircular-eval '(or false 2) the-global-environment) ;; 2

(display "(or false false)")(newline)
(metacircular-eval '(or false false) the-global-environment) ;; #f 

;derived expressions
(define (and->if exp)
  (expand-clauses-and (and-clauses exp)))
(define (expand-clauses-and clauses)
  (cond ((null? clauses) 'true)
        ((last-clause? clauses) (first-clause clauses))
        (else (make-if (first-clause clauses)
                       (expand-clauses-and (rest-clauses clauses))
                       'false))))
(define (or->if exp)
  (expand-clauses-or (or-clauses exp)))
(define (expand-clauses-or clauses)
  (cond ((null? clauses) 'false)
        (else (make-application
               (make-lambda '(exp)
                            (make-if 'exp
                                     'exp
                                     (expand-clauses-or (rest-clauses clauses))))
               (list (first-clause clauses))))))
(newline)
(display "Installing eval-and and eval-or as derived expressions")(newline)
(put 'eval 'and-der
     (lambda (exp env)
       (metacircular-eval (and->if exp) env)))
(put 'eval 'or-der
     (lambda (exp env)
       (metacircular-eval (or->if exp) env)))

(display "Converting to if expressions")(newline)

(newline)
(display "(and 1 2 3)")(newline)
(display "If expression: ") (and->if '(and 1 2 3)) ;; 2
(display "Result: ") (metacircular-eval '(and-der 1 2 3) the-global-environment)
;(metacircular-eval '(call and 1 2) the-global-environment)

(newline)
(display "(and false 2)")(newline)
(display "If expression: ") (and->if '(and false 2)) ;; false
(display "Result: ") (metacircular-eval '(and-der false 2) the-global-environment)

(newline)
(display "(or 1 2 3)")(newline)
(display "If expression: ") (or->if '(or 1 2 3)) ;; 1
(display "Result: ") (metacircular-eval '(or-der 1 2 3) the-global-environment)

(newline)
(display "(or false 2)")(newline)
(display "If expression: ") (or->if '(or false 2)) ;; 1
(display "Result: ") (metacircular-eval '(or-der false 2) the-global-environment)

(newline)
(display "(or false false)")(newline)
(display "If expression: ") (or->if '(or false false)) ;; 1
(display "Result: ") (metacircular-eval '(or-der false false) the-global-environment)

;Exercise 4.5
(newline)
(newline)
(display "Exercise 4.5")(newline)
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (apply-on-clause? clause)
  (eq? '=> (cadr clause)))
(define (cond-lambda clause) (caddr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses)))
              ((apply-on-clause? first)
               (make-application
                (make-lambda '(exp)
                             (make-if 'exp
                                      (make-application (cond-lambda first)
                                                        (list 'exp))
                                      (expand-clauses rest)))
                (list (cond-predicate first))))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))

(display "Converting to if expressions and evaluating with (define i 42)")(newline)
(newline)
(display "Normal conditional")(newline)
(display "(cond ((call > i 0) i)
                 ((call < i 0) (call * i -1))
                 (else 0))")(newline)
(display "If expression: ")(cond->if '(cond ((call > i 0) i)
                                            ((call < i 0) (call * i -1))
                                            (else 0)))
(display "Result: ")
(metacircular-eval (sequence->exp '((define i 42)
                                    (cond ((call > i 0) i)
                                          ((call < i 0) (call * i -1))
                                          (else 0))))
                   the-global-environment)

(newline)
(display "Extended conditional syntax")(newline)
(display "(cond ((call assoc 'b '((a 1) (b 2))) => cadr)
                 (else false))")(newline)
(display "If expression: ")(cond->if '(cond ((call assoc 'b '((a 1) (b 2))) => cadr)
                                            (else false)))
(display "Result: ")
(metacircular-eval '(cond ((call assoc 'b '((a 1) (b 2))) => cadr)
                          (else false))
                   the-global-environment)

;Exercise 4.6
(newline)
(display "Exercise 4.6")(newline)
(define (let? exp) (tagged-list? exp 'let))
(define (let-specs exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-spec-var spec-exp) (car spec-exp))
(define (let-spec-exp spec-exp) (cadr spec-exp))
(define (first-spec specs-exp) (car specs-exp))
(define (rest-specs specs-exp) (cdr specs-exp))
(define (list-of-vars specs-exp)
  (if (null? specs-exp)
      '()
      (cons (let-spec-var (first-spec specs-exp))
            (list-of-vars (rest-specs specs-exp)))))
(define (list-of-exps specs-exp)
  (if (null? specs-exp)
      '()
      (cons (let-spec-exp (first-spec specs-exp))
            (list-of-exps (rest-specs specs-exp)))))

(define (let->combination exp)
  (make-application
   (make-lambda (list-of-vars (let-specs exp))
                (sequence->exp (let-body exp)))
   (list-of-exps (let-specs exp))))

(newline)
(display "Installing let as a derived expression")(newline)
(put 'eval 'let
     (lambda (exp env)
       (metacircular-eval (let->combination exp) env))) 
(define (make-let list-specs body)
  (cons 'let (cons list-specs body)))
(display "Converting (let ((x 3) (y 2)) (call + (call * x x) y))")(newline)
(display "Derived expression: ")
(let->combination '(let ((x 3) (y 2))
                     (call + (call * x x) y)))
(display "Result: ")
(metacircular-eval '(let ((x 3) (y 2))
                     (call + (call * x x) y))
                   the-global-environment)

;Exercise 4.7
(newline)
(display "Exercise 4.7")(newline)
(define (let*? expr) (tagged-list? expr 'let*))
(define (let*->nested-lets exp)
  (expand-let* (let-specs exp) (let-body exp)))
(define (expand-let* specs body)
  (if (null? specs)
      body
      (let ((rest-body (expand-let* (rest-specs specs)
                                    body)))
        (if (null? (rest-specs specs))
            (make-let (list (first-spec specs))
                      rest-body)
            (make-let (list (first-spec specs))
                      (list rest-body))))))
(display "Installing let* as a derived expression")(newline)
(put 'eval 'let*
     (lambda (exp env)
       (metacircular-eval (let*->nested-lets exp) env)))
(display "Converting (let* ((x 3) (y 2)) (+ (* x x) y))")(newline)
(display "Derived expression (let): ")
(let*->nested-lets '(let* ((x 3) (y 2))
                     (call + (call * x x) y)))
(display "Derived expression: ")
(let->combination (let*->nested-lets '(let* ((x 3) (y 2))
                                        (call + (call * x x) y))))
(display "Result: ")
(metacircular-eval '(let* ((x 3) (y 2))
                      (call + (call * x x) y))
                   the-global-environment)

;Exercise 4.8
(newline)
(display "Exercise 4.8")(newline)
(define (named-let? exp)
  (not (pair? (let-specs exp))))
(define (named-let-name exp) (cadr exp))
(define (named-let-specs exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))
(define (make-definition var val) (list 'define var val))

(define (let->combination-alt exp)
  (if (named-let? exp)
      (let ((proc-def (make-lambda (list-of-vars (named-let-specs exp))
                                   (sequence->exp (named-let-body exp)))))
        (make-application (make-lambda '()
                                       (sequence->exp (list (make-definition (named-let-name exp) proc-def)
                                                            (make-application (named-let-name exp)
                                                                              (list-of-exps (named-let-specs exp))))))
                          '()))
      (make-application
       (make-lambda (list-of-vars (let-specs exp))
                    (sequence->exp (let-body exp)))
       (list-of-exps (let-specs exp)))))
(display "Installing named-let as a derived expression")(newline)
(put 'eval 'named-let
     (lambda (exp env)
       (metacircular-eval (let->combination-alt exp) env)))
(display "Converting (named-let fib-iter ((a 1) (b 0) (count 6)) (if (call = count 0) b (call fib-iter (call + a b) a (call - count 1)))))")(newline)
(display "Derived expression: ")
(let->combination-alt '(named-let fib-iter ((a 1)
                                            (b 0)
                                            (count 6))
                                  (if (call = count 0)
                                      b
                                      (call fib-iter (call + a b) a (call - count 1)))))
(display "Result Fib(6)=8: ")
(metacircular-eval '(named-let fib-iter ((a 1)
                                         (b 0)
                                         (count 6))
                               (if (call = count 0)
                                   b
                                   (call fib-iter (call + a b) a (call - count 1))))
      the-global-environment)

;Exercise 4.9
;(`do <actions> <until-predicate>)
;(`while <continue-predicate> <actions>)
;(`for (i 0) (< i N) (+ i 1) <actions>)

(define (do-until? exp) (tagged-list? exp 'do))
(define (do-actions exp) (cadr exp))
(define (until-predicate exp) (caddr exp))
(define (do-until->combination exp)
  (make-let (list (list 'do-until-iter
                        (make-lambda '()
                                     (sequence->exp (list (sequence->exp (do-actions exp))
                                                          (make-if (until-predicate exp)
                                                                   'ok
                                                                   (make-application 'do-until-iter '())))))))
            (make-application 'do-until-iter '())))

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-actions exp) (caddr exp))
(define (while->combination exp)
  (make-let (list (list 'while-iter
                        (make-lambda '()
                                     (sequence->exp (list (make-if (while-predicate exp)
                                                                   (sequence->exp (list (while-actions exp)
                                                                                        (make-application 'while-iter '())))
                                                                   'ok))))))
            (make-application 'while-iter '())))
  

(define (for? exp) (tagged-list? exp 'for))
(define (for-init exp) (cadr exp))
(define (for-end-predicate exp) (caddr exp))
(define (for-incrementor exp) (cadddr exp))
(define (for-actions exp) (cddddr exp))
;------------------TODO: for loop----------------------------------


;(driver-loop)