#lang sicp
(#%require "meta-lisp-evaluator-non-deterministic-simplified.scm")

(display "Exercise 4.45")(newline)
(display "Parsing - the professor lectures to the student in the class with the cat")(newline)
(ambtest '(begin
           (define (require p)
              (if (not p) (amb)))
           (define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items))))
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
             (require (not (null? *unparsed*)))
             (require (memq (car *unparsed*) (cdr word-list)))
             (let ((found-word (car *unparsed*)))
               (set! *unparsed* (cdr *unparsed*))
               (list (car word-list) found-word)))

           (define *unparsed* '())
           (define (parse input)
             (set! *unparsed* input)
             (let ((sent (parse-sentence)))
               (require (null? *unparsed*))
               sent))
           (parse '(the professor lectures to the student in the class with the cat)))
         #t
         1000)


(newline)(newline)
(display "Exercise 4.49")(newline)
(display "Generating ....")(newline)
(ambtest '(begin
           (define (require p)
              (if (not p) (amb)))
           (define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items))))
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
