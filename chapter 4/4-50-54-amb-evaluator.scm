#lang sicp
(#%require "meta-lisp-evaluator-non-deterministic.scm")
(#%require "meta-lisp-evaluator-non-deterministic-simplified.scm")

(display "Exercise 4.50")(newline)

(define (ramb-choices exp) (cdr exp))

(define (random-first-elem lis)
  (define (remove-elem lis n i)
    (if (= i n)
        (cdr lis)
        (cons (car lis)
              (remove-elem (cdr lis) n (+ i 1)))))
  (let ((x (random (length lis))))
    (cons (list-ref lis x) (remove-elem lis x 0))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((reordered-choices (random-first-elem choices)))
              ((car reordered-choices) env
                                       succeed
                                       (lambda ()
                                         (try-next (cdr reordered-choices)))))))
      (try-next cprocs))))

(put 'analyze 'ramb analyze-ramb)

(display "(amb 1 2 3 4) will always return 1 first") (newline)
(ambtest-2 '(amb 1 2 3 4) #t 10)(newline)

(newline)(newline)
(display "(ramb 1 2 3 4) can return 1, 2, 3 or 4 at random (rerun a few times)") (newline)
(ambtest-2 '(ramb 1 2 3 4) #t 10)(newline)


(newline)(newline)
(display "Generating more interesting sentences using ramb....")(newline)

(ambtest '(begin
           (define (require p)
              (if (not p) (amb)))
           (define (an-element-of items)
              (require (not (null? items)))
              (ramb (car items) (an-element-of (cdr items))))
           (define nouns '(noun student professor cat class))
           (define verbs '(verb studies lectures eats sleeps))
           (define articles '(article the a))
           (define prepositions '(prep for to in by with))
           (define (parse-prepositional-phrase)
             (list 'prep-phrase
                   (parse-word prepositions)
                   (parse-noun-phrase)))
           (define (parse-sentence)
             (list 'sentence
                   (parse-noun-phrase)
                   (parse-verb-phrase)))
           (define (parse-verb-phrase)
             (define (maybe-extend verb-phrase)
               (amb verb-phrase
                    (maybe-extend (list 'verb-phrase
                                        verb-phrase
                                        (parse-prepositional-phrase)))))
             (maybe-extend (parse-word verbs)))
           (define (parse-simple-noun-phrase)
             (list 'simple-noun-phrase
                   (parse-word articles)
                   (parse-word nouns)))
           (define (parse-noun-phrase)
             (define (maybe-extend noun-phrase)
               (amb noun-phrase
                    (maybe-extend (list 'noun-phrase
                                        noun-phrase
                                        (parse-prepositional-phrase)))))
             (maybe-extend (parse-simple-noun-phrase)))

            (define (parse-word word-list)
              (list (car word-list) (an-element-of (cdr word-list))))

           (define *unparsed* '())
           (define (parse input)
             (set! *unparsed* input)
             (let ((sent (parse-sentence)))
               (require (null? *unparsed*))
               sent))
           (parse '()))
         #t
         6)


(newline)(newline)(newline)
(display "Exercise 4.51")(newline)

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

(put 'analyze 'permanent-set! analyze-permanent-assignment)

(newline)
(display "Output using permanent-set!")(newline)
(ambtest-2 '(begin (define count 0)
                   (let ((x (call an-element-of '(a b c)))
                         (y (call an-element-of '(a b c))))
                     (permanent-set! count (call + count 1))
                     (call require (call not (call eq? x y)))
                     (call list x y count)))
           #t
           10)

(newline)
(display "Output using set!")(newline)
(ambtest-2 '(begin (define count 0)
                   (let ((x (call an-element-of '(a b c)))
                         (y (call an-element-of '(a b c))))
                     (set! count (call + count 1))
                     (call require (call not (call eq? x y)))
                     (call list x y count)))
           #t
           10)



(newline)(newline)(newline)
(display "Exercise 4.52")(newline)

(define (if-fail-success exp) (cadr exp))
(define (if-fail-fail exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((sproc (analyze (if-fail-success exp)))
        (fproc (analyze (if-fail-fail exp))))
    (lambda (env succeed fail)
      (sproc env
             succeed
             (lambda ()
               (fproc env succeed fail))))))
(put 'analyze 'if-fail analyze-if-fail)

(newline)
(display "Testing (1 3 5)")
(ambtest-2 '(if-fail (let ((x (call an-element-of '(1 3 5))))
                       (call require (call even? x))
                       x)
                     'all-odd)
           #t
           10)

(newline)(newline)
(display "Testing (1 3 5 8)")
(ambtest-2 '(if-fail (let ((x (call an-element-of '(1 3 5 8))))
                       (call require (call even? x))
                       x)
                     'all-odd)
           #t
           1)



(newline)(newline)(newline)
(display "Exercise 4.53")(newline)
(ambtest-2 '(let ((pairs '()))
              (if-fail (let ((p (call prime-sum-pair '(1 3 5 8) '(20 35 110))))
                         (permanent-set! pairs (call cons p pairs))
                         (amb))
                       pairs))
           #t
           10)



(newline)(newline)(newline)
(display "Exercise 4.54")(newline)

(define (require-predicate exp) (cadr exp))
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   fail2
                   (succeed 'ok fail2)))
             fail))))

(put 'analyze 'require analyze-require)
(ambtest-2
  '(begin
     (define an-integer-between
       (lambda (i j)
         (require (call <= i j))
         (amb i (call an-integer-between (call + i 1) j))))
     (let ((k (call an-integer-between 13 19)))
       (call display k))
     )
  #t
  1)