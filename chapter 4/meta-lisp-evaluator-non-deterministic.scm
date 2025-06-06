#lang sicp
(#%provide ambtest)
(#%provide ambeval)
(#%provide analyze)
(#%provide execute-application)
(#%provide apply-in-underlying-scheme)
(#%provide exp-type)
(#%provide self-evaluating?)
(#%provide variable?)
(#%provide text-of-quotation)
(#%provide assignment-variable)
(#%provide assignment-value)
(#%provide make-assignment)
(#%provide definition-variable)
(#%provide definition-value)
(#%provide lambda-parameters)
(#%provide lambda-body)
(#%provide make-lambda)
(#%provide if-predicate)
(#%provide if-consequent)
(#%provide if-alternative)
(#%provide make-if)
(#%provide begin-actions)
(#%provide last-exp?)
(#%provide first-exp)
(#%provide rest-exps)
(#%provide sequence->exp)
(#%provide make-begin)
(#%provide no-operands?)
(#%provide first-operand)
(#%provide rest-operands)
(#%provide make-application)
(#%provide cond-clauses)
(#%provide cond-else-clause?)
(#%provide cond-predicate)
(#%provide cond-actions)
(#%provide cond->if)
(#%provide expand-clauses)
(#%provide true?)
(#%provide false?)
(#%provide make-procedure)
(#%provide compound-procedure?)
(#%provide procedure-parameters)
(#%provide procedure-body)
(#%provide procedure-environment)
(#%provide enclosing-environment)
(#%provide first-frame)
(#%provide the-empty-environment)
(#%provide make-frame)
(#%provide frame-variables)
(#%provide frame-values)
(#%provide add-binding-to-frame!)
(#%provide extend-environment)
(#%provide lookup-variable-value)
(#%provide set-variable-value!)
(#%provide define-variable!)
(#%provide primitive-procedure?)
(#%provide primitive-implementation)
(#%provide primitive-procedures)
(#%provide primitive-procedure-names)
(#%provide primitive-procedure-objects)
(#%provide apply-primitive-procedure)
(#%provide setup-environment)
(#%provide the-global-environment)
(#%provide input-prompt)
(#%provide output-prompt)
(#%provide driver-loop)
(#%provide prompt-for-input)
(#%provide announce-output)
(#%provide user-print)
(#%provide make-table)
(#%provide get)
(#%provide put)

(define (ambtest expr all-vals max_reps)
  (let ((n 1))
    (ambeval expr
             the-global-environment
             (lambda (val next-alternative)
               (newline)
               (display ";;; Amb-Eval output\n")
               (display val)
               (newline)
               (if all-vals
                   (if (>= n max_reps)
                       (display "*** Reached max number of correct answers ***")
                       (begin
                         (set! n (+ n 1))
                         (next-alternative)))))
               (lambda ()
                 (display "*** No more values ***")))))

;;Table operations
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define apply-in-underlying-scheme apply)

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-val (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-val env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
             (lambda (val1 fail2)
               (proc2 env succeed fail2))
             fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application proc
                                                args
                                                succeed
                                                fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (exp-type exp)
  (cond ((self-evaluating? exp) 'self-eval)
        ((variable? exp) 'variable)
        ((and (pair? exp) (symbol? (car exp)))
         (car exp))
        (else (error "Unknown expression type -- EXP-TYPE" exp))))

(define (analyze exp)
  ;(display exp)(newline)
  (let ((op (get 'analyze (exp-type exp))))
    (if op
        (op exp)
        (error "Operation not found -- ANALYZE" exp))))

(define (install-syntax)
  (put 'analyze 'self-eval analyze-self-evaluating)
  (put 'analyze 'quote analyze-quoted)
  (put 'analyze 'variable analyze-variable)
  (put 'analyze 'set! analyze-assignment)
  (put 'analyze 'define analyze-definition)
  (put 'analyze 'if analyze-if)
  (put 'analyze 'lambda analyze-lambda)
  (put 'analyze 'if analyze-if)
  (put 'analyze 'let
       (lambda (exp) (analyze (let->combination exp))))
  (put 'analyze 'begin
       (lambda (exp) (analyze-sequence (begin-actions exp))))
  (put 'analyze 'call analyze-application)
  (put 'analyze 'cond
       (lambda (exp)
         (analyze (cond->if exp))))
  (put 'analyze 'amb analyze-amb)
  'ok)

(install-syntax)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (amb-choices exp) (cdr exp))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (make-assignment var val) (list 'set! var val))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (make-begin (cddr exp))))) ; body

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (list parameters body)))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (make-application procedure args)
  (cons 'call (cons procedure args)))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

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

(define (let-specs exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-spec-var spec-exp) (car spec-exp))
(define (let-spec-exp spec-exp) (cadr spec-exp))

(define (let->combination exp)
  (make-application
   (make-lambda (map let-spec-var (let-specs exp))
                (sequence->exp (let-body exp)))
   (map let-spec-exp (let-specs exp))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'caar caar)
        (list 'cons cons)
        (list 'list list)
        (list 'memq memq)
        (list 'eq? eq?)
        (list 'not not)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '- -)
        (list '< <)
        (list '= =)
        (list '> >)
        (list '<= <=)
        (list '>= >=)
        (list 'abs abs)
        (list 'odd? odd?)
        (list 'even? even?)
        (list 'null? null?)
        (list 'prime? prime?)
        (list 'display display)
        (list 'member member)
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


; Add analyze handlers for or/and
(define (analyze-or exp)
  (let ((procs (map analyze (cdr exp))))
    (lambda (env succeed fail)
      (define (try-next procs)
        (if (null? procs)
            (succeed false fail)
            ((car procs) env
                        (lambda (val fail2)
                          (if (true? val)
                              (succeed val fail2)
                              (try-next (cdr procs))))
                        fail)))
      (try-next procs))))

(define (analyze-and exp)
  (let ((procs (map analyze (cdr exp))))
    (lambda (env succeed fail)
      (define (try-next procs last-val)
        (if (null? procs)
            (succeed last-val fail)
            ((car procs) env
                        (lambda (val fail2)
                          (if (false? val)
                              (succeed false fail2)
                              (try-next (cdr procs) val)))
                        fail)))
      (try-next procs true))))

; Add to install-syntax
(put 'analyze 'or analyze-or)
(put 'analyze 'and analyze-and)


; Some function definitions that are useful to include in the global environment
(ambeval '(define require
            (lambda (p)
              (if (call not p) (amb))))
         the-global-environment
         (lambda (value fail) 'succeeded)
         (lambda () 'failed))

(ambeval '(define an-element-of
            (lambda (items)
              (call require (call not (call null? items)))
              (amb (call car items) (call an-element-of (call cdr items)))))
         the-global-environment
         (lambda (value fail) 'succeeded)
         (lambda () 'failed))

(ambeval '(define prime-sum-pair
            (lambda (list1 list2)
              (let ((a (call an-element-of list1))
                    (b (call an-element-of list2)))
                (call require (call prime? (call + a b)))
                (call list a b))))
         the-global-environment
         (lambda (value fail) 'succeeded)
         (lambda () 'failed))

;(driver-loop)

;(ambeval '(amb 1 2 3) the-global-environment (lambda (val fail) (display val)) (lambda () (display "Failed")))
