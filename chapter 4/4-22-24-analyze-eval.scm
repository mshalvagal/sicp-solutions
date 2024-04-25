#lang sicp
(#%require "meta-lisp-evaluator-analyze-eval.scm")

(display "Exercise 4.22")(newline)
(define (let-specs exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-spec-var spec-exp) (car spec-exp))
(define (let-spec-exp spec-exp) (cadr spec-exp))

(define (let->combination exp)
  (make-application
   (make-lambda (map let-spec-var (let-specs exp))
                (sequence->exp (let-body exp)))
   (map let-spec-exp (let-specs exp))))

(newline)
(display "Installing let as a derived expression")(newline)

(put 'analyze 'let
     (lambda (exp)
       (analyze (let->combination exp))))

(define (make-let list-specs body)
  (cons 'let (cons list-specs body)))

(newline)
(display "Converting (let ((x 3) (y 2)) (call + (call * x x) y))")(newline)

(newline)
(display "Derived expression: ")
(let->combination '(let ((x 3) (y 2))
                     (call + (call * x x) y)))

(newline)
(display "Result: ")
(metacircular-eval '(let ((x 3) (y 2))
                     (call + (call * x x) y))
                   the-global-environment)


(newline)
(display "Exercise 4.24")(newline)

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
(put 'analyze 'letrec
     (lambda (exp)
       (analyze (letrec->derived-let exp))))

(define start (runtime))
(define fact1000 (metacircular-eval '(letrec ((fact
          (lambda (n)
            (if (call = n 1)
                1
                (call * n (call fact (call - n 1)))))))
  (call fact 100000))
                   the-global-environment))
(define end (runtime))
(display "Time to compute: ")
(/ (- end start) 1000000.0)

