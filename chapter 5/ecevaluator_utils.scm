#lang sicp

(#%provide self-evaluating? quoted? text-of-quotation tagged-list?
           variable? assignment? assignment-variable assignment-value
           definition? definition-variable definition-value
           lambda? lambda-parameters lambda-body make-lambda
           if? if-predicate if-consequent if-alternative
           cond->if let->combination let? cond?
           first-clause-else? cond-clauses cond-actions first-clause-predicate first-clause-actions no-more-clauses? rest-clauses
           begin? begin-actions
           last-exp? first-exp rest-exps
           application? operator operands
           no-operands? first-operand rest-operands
           true? false?
           make-procedure compound-procedure? procedure-parameters procedure-body procedure-environment scan-out-defines
           enclosing-environment first-frame the-empty-environment
           make-frame frame-variables frame-values add-binding-to-frame!
           extend-environment lookup-variable-value set-variable-value! define-variable!
           setup-environment primitive-procedure? primitive-implementation
           primitive-procedures primitive-procedure-names primitive-procedure-objects
           apply-in-underlying-scheme apply-primitive-procedure
           prompt-for-input announce-output user-print
           empty-arglist adjoin-arg last-operand?
           compiled-procedure-entry compiled-procedure-env compiled-procedure? make-compiled-procedure
           compile-and-run? compile-and-run-expression
           no-more-exps? get-global-environment the-global-environment reset-global-environment!)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;following compound-procedure operations not used by compiled code
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
;;(end of compound procedures)


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	;;above from book -- here are some more
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ /)
	(list '> >)
	(list '< <)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

;;; Simulation of new machine operations needed by
;;;  eceval machine (not used by compiled code)

;;; From section 5.4.1 footnote
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;;; From section 5.4.2 footnote, for non-tail-recursive sequences
(define (no-more-exps? seq) (null? seq))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (no-more-clauses? exp) (null? exp))
(define (first-clause-predicate exp) (cond-predicate (car exp)))
(define (first-clause-actions exp) (cond-actions (car exp)))
(define (first-clause-else? exp) (cond-else-clause? (car exp)))
(define (rest-clauses exp) (cdr exp))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-specs exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-spec-var spec-exp) (car spec-exp))
(define (let-spec-exp spec-exp) (cadr spec-exp))

(define (let->combination exp)
  (make-application
   (make-lambda (map let-spec-var (let-specs exp))
                (let-body exp))
   (map let-spec-exp (let-specs exp))))

(define (make-application procedure args)
  (cons procedure args))

;;; From section 5.4.4 footnote
(define (get-global-environment)
  the-global-environment)
;; will do following when ready to run, not when load this file
(define the-global-environment (setup-environment))

(define (reset-global-environment!)
  (set! the-global-environment (setup-environment)))


;;; Exercise 5.43
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

(define (make-assignment var val) (list 'set! var val))
(define (make-let list-specs body)
  (cons 'let (cons list-specs body)))

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
        (list (make-let (map (lambda (var-name) (list var-name '*unassigned*))
                             defined-var-names)
                        proc-body)))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

;;; Exercise 5.48
(define (compile-and-run? exp) (tagged-list? exp 'compile-and-run))
(define (compile-and-run-expression exp) (text-of-quotation (cadr exp)))
