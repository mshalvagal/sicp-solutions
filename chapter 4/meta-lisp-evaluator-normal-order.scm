#lang sicp

(#%provide metacircular-eval)
(#%provide metacircular-apply)
(#%provide apply-in-underlying-scheme)
(#%provide eval-if)
(#%provide eval-sequence)
(#%provide eval-assignment)
(#%provide eval-definition)
(#%provide self-evaluating?)
(#%provide variable?)
(#%provide quoted?)
(#%provide text-of-quotation)
(#%provide tagged-list?)
(#%provide assignment?)
(#%provide assignment-variable)
(#%provide assignment-value)
(#%provide definition?)
(#%provide definition-variable)
(#%provide definition-value)
(#%provide lambda?)
(#%provide lambda-parameters)
(#%provide lambda-body)
(#%provide make-lambda)
(#%provide if?)
(#%provide if-predicate)
(#%provide if-consequent)
(#%provide if-alternative)
(#%provide make-if)
(#%provide begin?)
(#%provide begin-actions)
(#%provide last-exp?)
(#%provide first-exp)
(#%provide rest-exps)
(#%provide sequence->exp)
(#%provide make-begin)
(#%provide application?)
(#%provide no-operands?)
(#%provide first-operand)
(#%provide rest-operands)
(#%provide cond?)
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
(#%provide actual-value)
(#%provide force-it)



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


(define apply-in-underlying-scheme apply)

(define (actual-value exp env)
  (force-it (metacircular-eval exp env)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (exp-type exp) (car exp))
(define (metacircular-eval exp env)
  ;(display exp)(newline)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((and (pair? exp) (symbol? (car exp)))
         (let ((op (get 'eval (exp-type exp))))
           (if op
               (op exp env)
               (error "Operation not found -- EVAL" exp))))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (metacircular-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure
                                    (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-arg-values exps env)
  (map (lambda (exp) (actual-value exp env)) exps))

(define (list-of-delayed-args exps env)
  (map (lambda (exp) (delay-it exp env)) exps))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (metacircular-eval (if-consequent exp) env)
      (metacircular-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (metacircular-eval (first-exp exps) env))
        (else 
         (metacircular-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (metacircular-eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (metacircular-eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

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
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (sequence->exp (cddr exp))))) ; body

(define (make-definition var val) (list 'define var val))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (list parameters body)))

(define (if? exp) (tagged-list? exp 'if))
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

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (make-application procedure args)
  (cons 'call (cons procedure args)))

(define (cond? exp) (tagged-list? exp 'cond))
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
         (metacircular-apply (actual-value (operator exp) env)
                             (operands exp)
                             env)))
  'ok)


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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'caar caar)
        (list 'cons cons)
        (list 'null? null?)
        (list 'assoc assoc)
        ; weird alternate definition because scheme didn't let me use the set! primitive as an object
        (list 'set! (lambda (var val) (set! var val)))
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '- -)
        (list '< <)
        (list '= =)
        (list '> >)
        (list '<= <=)
        (list '>= >=)
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

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
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



(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(install-syntax)
(display "Testing (call * 2 2)")(newline)
(display "Result: ")(metacircular-eval '(call * 2 2) the-global-environment)
(display "Testing (call (lambda (x y) (+ (* x x) y)) 3 2)")(newline)
(metacircular-eval '(call (lambda (x y) (call + (call * x x) y)) 3 2) the-global-environment)


(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp) (cadddr exp))
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))
(put 'eval 'unless
     (lambda (exp env)
       (metacircular-eval (unless->if exp) env)))

(newline)
(display "Testing normal order evaluation")(newline)
(display "Testing (call (lambda (a b) (if (call = a 0) -42 b)) 0 (call / 1 0))")(newline)
(metacircular-eval '(call (lambda (a b)
                            (if (call = a 0) -42 b))
                          0
                          (call / 1 0))
                   the-global-environment)
;(driver-loop)