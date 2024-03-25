#lang sicp

(#%require "meta-lisp-evaluator-data-directed.scm")
(#%require "4-01-10-evaluator-basics.scm")

(display "Exercise 4.16")(newline)
(newline)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? val '*unassigned)
                   (error "Attempted to access unassigned variable in LOOKUP-VARIABLE-VALUE" var)
                   (car vals))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (make-assignment var val) (list 'set! var val))

(define (scan-out-defines proc-body)
  (define (modify-body! remaining-exps)
    (if (null? remaining-exps)
        '()
        (let ((first-exp (car remaining-exps)))
          (if (definition? first-exp)
              (begin
                (set-car! remaining-exps
                          (make-assignment (definition-variable first-exp)
                                           (definition-value first-exp)))
                (cons (definition-variable first-exp)
                      (modify-body! (cdr remaining-exps))))
              (modify-body! (cdr remaining-exps))))))
  (let ((defined-var-names (modify-body! proc-body)))
    (if (null? defined-var-names)
        proc-body
        (make-let (map (lambda (var-name) (list var-name '*unassigned*))
                       defined-var-names)
                  proc-body))))

(define (procedure-body p)
  (scan-out-defines (caddr p)))

(display "Scanning out defines from ((define u 1)
                    (define v 2)
                    (call + u v)
                    (call - u v))")(newline)
(scan-out-defines `((define u 1)
                    (define v 2)
                    (call + u v)
                    (call - u v)))

(newline)
(display "Scanning out defines from ((call + u v)
                    (call - u v))")(newline)
(scan-out-defines `((call + u v)
                    (call - u v)))

(newline)
(display "Exercise 4.17")(newline)
(define (scan-out-defines-same-frame proc-body)
  (define (modify-body! remaining-exps)
    (if (null? remaining-exps)
        '()
        (let ((first-exp (car remaining-exps)))
          (if (definition? first-exp)
              (begin
                (set-car! remaining-exps
                          (make-assignment (definition-variable first-exp)
                                           (definition-value first-exp)))
                (cons (definition-variable first-exp)
                      (modify-body! (cdr remaining-exps))))
              (modify-body! (cdr remaining-exps))))))
  (let ((defined-var-names (modify-body! proc-body)))
    (if (null? defined-var-names)
        proc-body
        (append (map (lambda (var-name) (make-definition var-name '*unassigned*))
                     defined-var-names)
                proc-body))))
(display "Scanning out defines from ((define u 1)
                    (define v 2)
                    (call + u v)
                    (call - u v))")(newline)
(scan-out-defines-same-frame `((define u 1)
                               (define v 2)
                               (call + u v)
                               (call - u v)))


(newline)
(display "Exercise 4.20")(newline)

(define (letrec-specs exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec-spec-var spec-exp) (car spec-exp))
(define (letrec-spec-exp spec-exp) (cadr spec-exp))

(define (letrec->derived-let exp)
  (make-let (map
              (lambda (var-name) (list var-name ''*unassigned*))
              (map
               letrec-spec-var
               (letrec-specs exp)))
            (append (map (lambda (spec)
                           (make-assignment (letrec-spec-var spec)
                                            (letrec-spec-exp spec)))
                         (letrec-specs exp))
                    (letrec-body exp))))
(put 'eval 'letrec
     (lambda (exp env)
       (metacircular-eval (letrec->derived-let exp) env))) 
(display "Derived let expression from (letrec ((fact
          (lambda (n)
            (if (call = n 1)
                1
                (call * n (call fact (call - n 1)))))))
  (call fact 10))")(newline)
(letrec->derived-let '(letrec ((fact
          (lambda (n)
            (if (call = n 1)
                1
                (call * n (call fact (call - n 1)))))))
  (call fact 10)))
(display "Result (10!): ")
(metacircular-eval '(letrec ((fact
          (lambda (n)
            (if (call = n 1)
                1
                (call * n (call fact (call - n 1)))))))
  (call fact 10))
                   the-global-environment)

(newline)
(display "Exercise 4.21")(newline)
(display "Y-combinator fact(10) = ")
((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(display "Y-combinator even?(2) = ")
(f 2)
(display "Y-combinator even?(31) = ")
(f 31)
(display "Y-combinator even?(42) = ")
(f 42)

(newline)
(display "Reference time for exercise 4.24")(newline)
(define start (runtime))
(define fact1000 (metacircular-eval '(letrec ((fact
          (lambda (n)
            (if (call = n 1)
                1
                (call * n (call fact (call - n 1)))))))
  (call fact 100000))
                   the-global-environment))
(define end (runtime))
(display "Time to compute fact(100000): ")
(/ (- end start) 1000000.0)