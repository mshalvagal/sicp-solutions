#lang sicp

(#%require "ecevaluator_utils.scm")

(#%provide compile-efficient statements)

;; 5.5.1
(define (compile-efficient exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assigment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((cond? exp)
         (compile-efficient (cond->if exp) target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((let? exp)
         (compile-efficient (let->combination exp) target linkage))
        ((open-add? exp)
         (compile-open-add exp target linkage))
        ((open-sub? exp)
         (compile-open-sub exp target linkage))
        ((open-mul? exp)
         (compile-open-mul exp target linkage))
        ((open-eq? exp)
         (compile-open-eq exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
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

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '(env) (list target)
                              `((assign ,target
                                        (op lookup-variable-value)
                                        (const ,exp)
                                        (reg env))))))

(define (compile-assigment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile-efficient (assignment-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence '(env val) (list target)
                                            `((perform (op set-variable-value!)
                                                       (const ,var)
                                                       (reg val)
                                                       (reg env))
                                              (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile-efficient (definition-value exp) 'val 'next)))
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


(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile-efficient (if-predicate exp) 'val 'next))
            (c-code
             (compile-efficient
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile-efficient
              (if-alternative exp) target linkage)))
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

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile-efficient (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile-efficient (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage)
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
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
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
     (compile-sequence (lambda-body exp) 'val 'return))))

;; 5.5.3
(define (compile-application exp target linkage)
  (let ((proc-code (compile-efficient (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile-efficient operand 'val 'next))
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
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
                                     `((assign ,target
                                               (op apply-primitive-procedure)
                                               (reg proc)
                                               (reg argl)))))))
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

;;Exercise 5.38
(define (spread-arguments operand-list)
  (let ((operand-1-code (compile-efficient (car operand-list) 'arg1 'next))
        (operand-2-code (compile-efficient (cadr operand-list) 'arg2 'next)))
    (preserving '(env)
                operand-1-code
                (preserving '(arg1)
                            operand-2-code
                            (make-instruction-sequence '(arg1 arg2) '() '())))))

(define (open-add? exp)
  (tagged-list? exp '+))
(define (open-sub? exp)
  (and (tagged-list? exp '-)
       (= 2 (length (operands exp)))))
(define (open-mul? exp)
  (tagged-list? exp '*))
(define (open-eq? exp)
  (and (tagged-list? exp '=)
       (= 2 (length (operands exp)))))

(define (compile-open-add exp target linkage)
  (let ((ops (operands exp)))
    (cond ((null? (cdr ops))
           (compile-efficient (car ops) target linkage))
          ((null? (cddr ops))
           (end-with-linkage
            linkage
            (append-instruction-sequences (spread-arguments ops)
                                          (make-instruction-sequence '(arg1 arg2) (list target)
                                                                     `((assign ,target (op +) (reg arg1) (reg arg2)))))))
          (else
           (compile-open-add (split-open-op exp) target linkage)))))

(define (compile-open-sub exp target linkage)
  (end-with-linkage
   linkage 
   (append-instruction-sequences (spread-arguments (operands exp))
                                 (make-instruction-sequence '(arg1 arg2) (list target)
                                                            `((assign ,target (op -) (reg arg1) (reg arg2)))))))

(define (compile-open-mul exp target linkage)
  (let ((ops (operands exp)))
    (cond ((null? (cdr ops))
           (compile-efficient (car ops) target linkage))
          ((null? (cddr ops))
           (end-with-linkage
            linkage
            (append-instruction-sequences (spread-arguments ops)
                                          (make-instruction-sequence '(arg1 arg2) (list target)
                                                                     `((assign ,target (op *) (reg arg1) (reg arg2)))))))
          (else
           (compile-open-mul (split-open-op exp) target linkage)))))

(define (compile-open-eq exp target linkage)
  (end-with-linkage
   linkage 
   (append-instruction-sequences (spread-arguments (operands exp))
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
