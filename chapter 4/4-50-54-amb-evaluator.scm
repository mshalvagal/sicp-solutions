#lang sicp
(#%require "meta-lisp-evaluator-non-deterministic.scm")

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
(ambtest '(amb 1 2 3 4) #t 10)(newline)

(newline)(newline)
(display "(ramb 1 2 3 4) can return 1, 2, 3 or 4 at random (rerun a few times)") (newline)
(ambtest '(ramb 1 2 3 4) #t 10)(newline)


(newline)(newline)
(display "Generating more interesting sentences using ramb....")(newline)

(ambtest '(begin
            (define (require p)
              (if (call not p) (amb)))
            (define (an-element-of items)
              (call require (call not (call null? items)))
              (ramb (call car items) (call an-element-of (call cdr items))))
            (define nouns '(noun student professor cat class))
            (define verbs '(verb studies lectures eats sleeps))
            (define articles '(article the a))
            (define prepositions '(prep for to in by with))
            (define (parse-prepositional-phrase)
              (call list 'prep-phrase
                    (call parse-word prepositions)
                    (call parse-noun-phrase)))
            (define (parse-sentence)
              (call list 'sentence
                    (call parse-noun-phrase)
                    (call parse-verb-phrase)))
            (define (parse-verb-phrase)
              (define (maybe-extend verb-phrase)
                (amb verb-phrase
                     (call maybe-extend (call list 'verb-phrase
                                              verb-phrase
                                              (call parse-prepositional-phrase)))))
              (call maybe-extend (call parse-word verbs)))
            (define (parse-simple-noun-phrase)
              (call list 'simple-noun-phrase
                    (call parse-word articles)
                    (call parse-word nouns)))
            (define (parse-noun-phrase)
              (define (maybe-extend noun-phrase)
                (amb noun-phrase
                     (call maybe-extend (call list 'noun-phrase
                                              noun-phrase
                                              (call parse-prepositional-phrase)))))
              (call maybe-extend (call parse-simple-noun-phrase)))

            (define (parse-word word-list)
              (call list (call car word-list) (call an-element-of (call cdr word-list))))

            (define *unparsed* '())
            (define (parse input)
              (set! *unparsed* input)
              (let ((sent (call parse-sentence)))
                (call require (call null? *unparsed*))
                sent))
            (call parse '()))
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
(ambtest '(begin (define count 0)
                 (let ((x (call an-element-of '(a b c)))
                       (y (call an-element-of '(a b c))))
                   (permanent-set! count (call + count 1))
                   (call require (call not (call eq? x y)))
                   (call list x y count)))
         #t
         10)

(newline)
(display "Output using set!")(newline)
(ambtest '(begin (define count 0)
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
(ambtest '(if-fail (let ((x (call an-element-of '(1 3 5))))
                     (call require (call even? x))
                     x)
                   'all-odd)
         #t
         10)

(newline)(newline)
(display "Testing (1 3 5 8)")
(ambtest '(if-fail (let ((x (call an-element-of '(1 3 5 8))))
                     (call require (call even? x))
                     x)
                   'all-odd)
         #t
         1)



(newline)(newline)(newline)
(display "Exercise 4.53")(newline)
(ambtest '(let ((pairs '()))
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
(ambtest
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