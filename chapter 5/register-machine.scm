#lang sicp

(#%provide make-machine)
(#%provide set-register-contents!)
(#%provide get-register-contents)
(#%provide start)
(#%provide trace-register)
(#%provide stop-trace-register)
(#%provide set-breakpoint)
(#%provide cancel-breakpoint)
(#%provide cancel-all-breakpoints)

; model of the machine is represented as a procedure with local state.

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine))) ; creating one empty machine model
    #|Exercise 5.13 - registers are auto-allocated from the source code
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name)) ; allocating registers in the machine
              register-names)
    |#
    ((machine 'install-operations) ops) ; passing a message to install operations
    ((machine 'install-instruction-sequence) ; passing a message to install instruction sequenc
     (assemble controller-text machine)) ; instruction sequence is converted to the machine representation
    machine))

; register is a procedure with the local state and set of messages that it answers to.

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-instructions #f))
    (define (set-register-value! value)
      (if trace-instructions
          (begin
            (display "Register: ") (display name)
            (display "\tOld value: ") (display contents)
            (display "\tNew value: ") (display value)(newline)))
      (set! contents value))
    
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) set-register-value!)
            ((eq? message 'trace-on) (set! trace-instructions #t))
            ((eq? message 'trace-off) (set! trace-instructions #f))
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

; stack implementation as a procedure with local state
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (add-unique element lst)
  (if (member element lst)
      lst                      ; Return the original list if element is already present
      (cons element lst)))     ; Otherwise add the element to the front of the list

; implementation of the model of the machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (sorted-instruction-list '())
        (entry-points-list '())
        (stack-registers '())
        (inst-exec-count 0)
        (trace-instructions #f)
        (breakpoints '())
        (continue-from-breakpoint #f))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag)))
          (register-source-table
            (list (list 'pc) (list 'flag))))

      ; get the value of the register
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            ;Exercise 5.13 - registers are auto-allocated from the source code
            (begin
              (set! register-source-table (cons (list name)
                                                register-source-table))
              (let ((new-reg (make-register name)))
                (set! register-table
                      (cons (list name new-reg)
                            register-table))
                new-reg)))))

      ;Exercise 5.12
      (define (log-register-source register-name register-source)
        (let ((pair (assoc register-name register-source-table)))
          (if pair
              (if (null? (cdr pair))
                  ;; If no sources yet, create the list
                  (set-cdr! pair (list register-source))
                  ;; Otherwise add uniquely to existing list
                  (set-cdr! pair (add-unique register-source (cdr pair))))
              (error "Unknown register -- STORE-REGISTER-SOURCE" register-name))))

      ; run the machine
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              (set! inst-exec-count (+ inst-exec-count 1))
              (let* ((inst (car insts))
                     (preceding-label (instruction-preceding-label inst))
                     (preceding-label-name (inst-label-name preceding-label))
                     (line-num (inst-label-count preceding-label)))
                (if trace-instructions
                    (begin
                      (if (= line-num 1)
                          (begin
                            (display preceding-label-name) (newline)))
                      (display (instruction-text inst)) (newline)))
                ((instruction-execution-proc inst)))
              (execute)))))

      ;set breakpoints
      (define (add-breakpoint label n)
        (set! breakpoints (cons (list label n) breakpoints)))

      ;remove breakpoints
      (define (remove-breakpoint label n)
        (set! breakpoints (filter (lambda (breakpoint)
                                    (not (eq? breakpoint (list label n))))
                                  breakpoints)))

      ;remove all breakpoints
      (define (remove-all-breakpoints)
        (set! breakpoints '()))

      ; external interface
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ;((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;logging operations
              ((eq? message 'store-instructions)
               (lambda (inst-list) (set! sorted-instruction-list inst-list)))
              ((eq? message 'add-entry-point)
               (lambda (entry-point)
                 (set! entry-points-list (add-unique entry-point entry-points-list))))
              ((eq? message 'add-stack-register)
               (lambda (stack-register) (set! stack-registers (add-unique stack-register stack-registers))))
              ((eq? message 'store-register-source) log-register-source)
              ((eq? message 'get-instructions) sorted-instruction-list)
              ((eq? message 'get-entry-points) entry-points-list)
              ((eq? message 'get-stack-registers) stack-registers)
              ((eq? message 'get-register-sources) register-source-table)
              ;tracing operations
              ((eq? message 'get-exec-count)
               (let ((temp inst-exec-count))
                 (set! inst-exec-count 0)
                 temp))
              ((eq? message 'trace-on) (set! trace-instructions #t))
              ((eq? message 'trace-off) (set! trace-instructions #f))
              ((eq? message 'trace-reg-on)
               (lambda (reg-name) ((lookup-register reg-name) 'trace-on)))
              ((eq? message 'trace-reg-off)
               (lambda (reg-name) ((lookup-register reg-name) 'trace-off)))
              ;breakpoint operations
              ((eq? message 'add-breakpoint) add-breakpoint)
              ((eq? message 'remove-breakpoint) remove-breakpoint)
              ((eq? message 'remove-all-breakpoints) remove-all-breakpoints)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name) (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name) ((machine 'get-register) reg-name))
(define (trace-register machine reg-name) ((machine 'trace-reg-on) reg-name))
(define (stop-trace-register machine reg-name) ((machine 'trace-reg-off) reg-name))
(define (set-breakpoint machine label n) ((machine 'add-breakpoint) label n))
(define (cancel-breakpoint machine label n) ((machine 'remove-breakpoint) label n))
(define (cancel-all-breakpoints machine) ((machine 'remove-all-breakpoints)))


; assembler is converting list of controller steps to a set of executable instructions

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
              (let ((val (assoc next-inst labels)))
                (if val
                    (error "Multiple definitions of label -- EXTRACT-LABELS" next-inst)
                    (receive (attach-label insts next-inst)
                             (cons (make-label-entry next-inst insts)
                                   labels))))
              (receive (cons (make-instruction next-inst)
                             insts)
                       labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ((machine 'store-instructions) (get-sorted-instructions insts))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))


;store the sorted instructions in a table
(define (get-sorted-instructions insts)
  (let* ((inst-table (make-table))
         (get (inst-table 'lookup-proc))
         (put (inst-table 'insert-proc!)))
    (for-each (lambda (inst)
                (let ((inst-text (instruction-text inst)))
                  (put (inst-type inst-text)
                       (inst-args inst-text)
                       inst-text)))
              insts)
    (inst-table 'get-all-values)))
(define (inst-type inst) (car inst))
(define (inst-args inst) (cdr inst))

(define (attach-label insts label)
  (define (attach-label-iter rest-insts label n)
    (if (null? rest-insts)
        rest-insts
        (let ((next-inst (car rest-insts)))
          (if (or (null? next-inst)
                  (not (null? (cdr next-inst))))
              'done
              (begin
                (set-cdr! next-inst (list label n))
                (attach-label-iter (cdr rest-insts)
                                   label
                                   (+ n 1)))))))
  (attach-label-iter insts label 1)
  insts)
(define (inst-label-count inst-label)
  (cadr inst-label))
(define (inst-label-name inst-label)
  (car inst-label))

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cadr inst))
(define (instruction-preceding-label inst)
  (cddr inst))
(define (set-instruction-execution-proc! inst proc)
  (let ((prev-label (cdr inst)))
    (set-cdr! inst (cons proc prev-label))))


(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

; we have to make execution procedure for every machine instruction.

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

; assignment procedure
(define (make-assign inst machine labels operations pc)
  (let* ((target-reg-name (assign-reg-name inst))
         (target (get-register machine target-reg-name))
         (value-exp (assign-value-exp inst)))
    ((machine 'store-register-source) target-reg-name value-exp)
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

; testing
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;branching
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

; goto
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             ((machine 'add-entry-point) (register-exp-reg dest)) ;log entry point registers
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

; save to stack execution procedure
(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    ((machine 'add-stack-register) reg-name)
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

; restore from stack execution procedure
(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    ((machine 'add-stack-register) reg-name)
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

; perform an action
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

; primitive expressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

; primitive expressions excluding labels
(define (make-primitive-exp-no-label exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Invalid expression type -- ASSEMBLE-OP" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

; operation expression
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp-no-label e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))


(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;Exercise 5.12

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
    ;; Add this function to get all values as a flat list
    (define (get-all-values)
      (let loop ((entries (cdr local-table))
                 (result '()))
        (if (null? entries)
            result
            (let* ((entry (car entries))
                   (key1 (car entry))
                   (records (cdr entry)))
              (loop (cdr entries)
                    (append result
                            (map cdr records)))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'get-all-values) (get-all-values))  ;; New message
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;Test GCD machine
(define gcd-machine
  (make-machine
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(display "GCD(206,40) = ")(get-register-contents gcd-machine 'a)
(newline)
(display "Stack statistics")
((gcd-machine 'stack) 'print-statistics)(newline)
(display "Instruction count")(newline)
(gcd-machine 'get-exec-count)

(newline)(newline)
(display "Sorted instructions:\n")
(gcd-machine 'get-instructions)
(newline)
(display "Entry points:\n")
(gcd-machine 'get-entry-points)
(newline)
(display "Stack registers:\n")
(gcd-machine 'get-stack-registers)
(newline)
(display "Register source lists:\n")
(gcd-machine 'get-register-sources)