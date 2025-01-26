#lang sicp
(#%require "meta-lisp-evaluator-non-deterministic.scm")

(display "Exercise 4.45")(newline)
(display "Parsing - the professor lectures to the student in the class with the cat")(newline)
(ambtest '(begin
           (define (require p)
              (if (call not p) (amb)))
           (define (an-element-of items)
              (call require (call not (call null? items)))
              (amb (call car items) (call an-element-of (call cdr items))))
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
             (call require (call not (call null? *unparsed*)))
             (call require (call memq (call car *unparsed*) (call cdr word-list)))
             (let ((found-word (call car *unparsed*)))
               (set! *unparsed* (call cdr *unparsed*))
               (call list (call car word-list) found-word)))

           (define *unparsed* '())
           (define (parse input)
             (set! *unparsed* input)
             (let ((sent (call parse-sentence)))
               (call require (call null? *unparsed*))
               sent))
           (call parse '(the professor lectures to the student in the class with the cat)))
         #t
         1000)


(newline)(newline)
(display "Exercise 4.49")(newline)
(display "Generating ....")(newline)
(ambtest '(begin
            (define (require p)
              (if (call not p) (amb)))
            (define (an-element-of items)
              (call require (call not (call null? items)))
              (amb (call car items) (call an-element-of (call cdr items))))
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
