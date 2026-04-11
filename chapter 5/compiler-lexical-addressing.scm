#lang sicp

(#%require "ecevaluator_utils.scm")
(#%require "lexical-addressing-utils.scm")

(#%provide compile statements)

;; 5.5.1
(define (compile exp target linkage cenv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage cenv))
        ((assignment? exp)
         (compile-assigment exp target linkage cenv))
        ((definition? exp)
         (compile-definition exp target linkage cenv))
        ((if? exp)
         (compile-if exp target linkage cenv))
        ((let? exp)
         (compile-let exp target linkage cenv))
        ((lambda? exp)
         (compile-lambda exp target linkage cenv))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           cenv))
        ((open-add? exp cenv) (compile-open-add exp target linkage cenv))
        ((open-sub? exp cenv) (compile-open-sub exp target linkage cenv))
        ((open-mul? exp cenv) (compile-open-mul exp target linkage cenv))
        ((open-eq? exp cenv)  (compile-open-eq exp target linkage cenv))
        ((application? exp)
         (compile-application exp target linkage cenv))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;; 5.5.2
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage cenv)
  (end-with-linkage
   linkage
   (let ((address (find-variable exp cenv)))
     (if (eq? address 'not-found)
         (make-instruction-sequence '(env) (list target)
                                    `((assign ,target
                                              (op lookup-variable-value)
                                              (const ,exp)
                                              (reg env))))
         (make-instruction-sequence '(env) (list target)
                                    `((assign ,target
                                              (op lexical-address-lookup)
                                              (const ,address)
                                              (reg env))))))))


;; Exercise 5.41
(define (find-variable var cenv)
  (define (var-loop vars i)
    (cond ((null? vars) 'not-found)
          ((eq? (car vars) var) i)
          (else (var-loop (cdr vars) (+ i 1)))))
  (define (env-loop env n)
    (if (null? env)
        'not-found
        (let ((var-idx (var-loop (car env) 0)))
          (if (eq? var-idx 'not-found)
              (env-loop (cdr env) (+ n 1))
              (make-lexical-address n var-idx)))))
  (env-loop cenv 0))

;(find-variable 'c '((y z) (a b c d e) (x y)))
;(find-variable 'x '((y z) (a b c d e) (x y)))
;(find-variable 'w '((y z) (a b c d e) (x y)))

(define (compile-assigment exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next cenv)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (let ((address (find-variable var cenv)))
                   (if (eq? address 'not-found)
                       (make-instruction-sequence '(env val) (list target)
                                                  `((perform (op set-variable-value!)
                                                             (const ,var)
                                                             (reg val)
                                                             (reg env))
                                                    (assign ,target (const ok))))
                       (make-instruction-sequence '(env val) (list target)
                                                  `((perform (op lexical-address-set!)
                                                             (const ,address)
                                                             (reg val)
                                                             (reg env))
                                                    (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage cenv)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next cenv)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence '(env val) (list target)
                                            `((perform (op define-variable!)
                                                       (const ,var)
                                                       (reg val)
                                                       (reg env))
                                              (assign ,target (const ok))))))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))
(define (reset-label-counter)
  (set! label-counter 0))


(define (compile-if exp target linkage cenv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next cenv))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage cenv))
            (a-code
             (compile
              (if-alternative exp) target linkage cenv)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(define (compile-sequence seq target linkage cenv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage cenv)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next cenv)
                  (compile-sequence (rest-exps seq) target linkage cenv))))

(define (compile-lambda exp target linkage cenv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence '(env) (list target)
                                    `((assign ,target
                                              (op make-compiled-procedure)
                                              (label ,proc-entry)
                                              (reg env)))))
        (compile-lambda-body exp proc-entry cenv))
       after-lambda))))

(define (compile-lambda-body exp proc-entry cenv)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
                                `(,proc-entry
                                  (assign env (op compiled-procedure-env) (reg proc))
                                  (assign env
                                          (op extend-environment)
                                          (const ,formals)
                                          (reg argl)
                                          (reg env))))
     ;; Exercise 5.40, 5.43
     (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return
                       (extend-cenv formals cenv)))))

(define (extend-cenv formals cenv)
  (cons formals cenv))

;; 5.5.3
(define (compile-application exp target linkage cenv)
  (let ((proc-code (compile (operator exp) 'proc 'next cenv))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next cenv))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence '(val argl) '(argl)
                                                '((assign argl
                                                          (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage))
          (compound-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (make-instruction-sequence '(proc) '()
                                  `((test (op compound-procedure?) (reg proc))
                                    (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (parallel-instruction-sequences
         (append-instruction-sequences
          compound-branch
          (compound-proc-appl target compound-linkage))
         (append-instruction-sequences
          primitive-branch
          (end-with-linkage
           linkage
           (make-instruction-sequence '(proc argl)
                                      (list target)
                                      `((assign ,target
                                                (op apply-primitive-procedure)
                                                (reg proc)
                                                (reg argl))))))))
       after-call))))

(define all-regs '(env proc val argl continue arg1 arg2))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
                                    '((assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc compapp) all-regs
                                    `((assign continue (label ,linkage))
                                      (save continue)
                                      (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc compapp) all-regs
                                      `((assign continue (label ,proc-return))
                                        (save continue)
                                        (goto (reg compapp))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue compapp) all-regs
                                    '((save continue)
                                      (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE COMPOUND"
                target))))

;; 5.5.4
(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))


(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))
;; not used, but for reference: list-intersection, list-symmetric-difference
(define (list-intersection s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (cons (car s1) (list-intersection (cdr s1) s2)))
        (else (list-intersection (cdr s1) s2))))
(define (list-symmetric-difference s1 s2)
  (list-difference (list-union s1 s2) (list-intersection s1 s2)))


(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (compile-let exp target linkage cenv)
  (compile (let->combination exp) target linkage cenv))


;;Exercise 5.38
(define (spread-arguments operand-list cenv)
  (let ((operand-1-code (compile (car operand-list) 'arg1 'next cenv))
        (operand-2-code (compile (cadr operand-list) 'arg2 'next cenv)))
    (preserving '(env)
                operand-1-code
                (preserving '(arg1)
                            operand-2-code
                            (make-instruction-sequence '(arg1 arg2) '() '())))))

(define (open-add? exp cenv)
  (and (tagged-list? exp '+)
       (eq? (find-variable '+ cenv) 'not-found)))
(define (open-sub? exp cenv)
  (and (tagged-list? exp '-)
       (= 2 (length (operands exp)))
       (eq? (find-variable '- cenv) 'not-found)))
(define (open-mul? exp cenv)
  (and (tagged-list? exp '*)
       (eq? (find-variable '* cenv) 'not-found)))
(define (open-eq? exp cenv)
  (and (tagged-list? exp '=)
       (= 2 (length (operands exp)))
       (eq? (find-variable '= cenv) 'not-found)))

(define (compile-open-add exp target linkage cenv)
  (let ((ops (operands exp)))
    (cond ((null? (cdr ops))
           (compile (car ops) target linkage cenv))
          ((null? (cddr ops))
           (end-with-linkage
            linkage
            (append-instruction-sequences (spread-arguments ops cenv)
                                          (make-instruction-sequence '(arg1 arg2) (list target)
                                                                     `((assign ,target (op +) (reg arg1) (reg arg2)))))))
          (else
           (compile-open-add (split-open-op exp) target linkage cenv)))))

(define (compile-open-sub exp target linkage cenv)
  (end-with-linkage
   linkage 
   (append-instruction-sequences (spread-arguments (operands exp) cenv)
                                 (make-instruction-sequence '(arg1 arg2) (list target)
                                                            `((assign ,target (op -) (reg arg1) (reg arg2)))))))

(define (compile-open-mul exp target linkage cenv)
  (let ((ops (operands exp)))
    (cond ((null? (cdr ops))
           (compile (car ops) target linkage cenv))
          ((null? (cddr ops))
           (end-with-linkage
            linkage
            (append-instruction-sequences (spread-arguments ops cenv)
                                          (make-instruction-sequence '(arg1 arg2) (list target)
                                                                     `((assign ,target (op *) (reg arg1) (reg arg2)))))))
          (else
           (compile-open-mul (split-open-op exp) target linkage cenv)))))

(define (compile-open-eq exp target linkage cenv)
  (end-with-linkage
   linkage 
   (append-instruction-sequences (spread-arguments (operands exp) cenv)
                                 (make-instruction-sequence '(arg1 arg2) (list target)
                                                            `((assign ,target (op =) (reg arg1) (reg arg2)))))))

(define (split-open-op exp)
  (let ((op (operator exp))
        (ops (operands exp)))
    (if (null? (cddr ops))
        exp
        (list op
              (car ops)
              (split-open-op (cons op (cdr ops)))))))
