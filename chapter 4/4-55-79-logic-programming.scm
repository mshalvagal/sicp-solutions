#lang sicp
(#%require "logic-interpreter.scm")
(#%require "logic-interpreter-no-loop-detect.scm")

(display "TESTING EXAMPLE DATABASE")
(newline)
(newline)

(display "Section 4.4.1")
(newline)
(newline)

(query-driver-loop-for-script
  '((assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
    (assert! (job (Bitdiddle Ben) (computer wizard)))
    (assert! (salary (Bitdiddle Ben) 60000))

    (assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
    (assert! (job (Hacker Alyssa P) (computer programmer)))
    (assert! (salary (Hacker Alyssa P) 40000))
    (assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
    (assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
    (assert! (job (Fect Cy D) (computer programmer)))
    (assert! (salary (Fect Cy D) 35000))
    (assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
    (assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
    (assert! (job (Tweakit Lem E) (computer technician)))
    (assert! (salary (Tweakit Lem E) 25000))
    (assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

    (assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
    (assert! (job (Reasoner Louis) (computer programmer trainee)))
    (assert! (salary (Reasoner Louis) 30000))
    (assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

    (assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
    (assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
    (assert! (job (Warbucks Oliver) (administration big wheel)))
    (assert! (salary (Warbucks Oliver) 150000))

    (assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
    (assert! (job (Scrooge Eben) (accounting chief accountant)))
    (assert! (salary (Scrooge Eben) 75000))
    (assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
    (assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
    (assert! (job (Cratchet Robert) (accounting scrivener)))
    (assert! (salary (Cratchet Robert) 18000))
    (assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

    (assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
    (assert! (job (Aull DeWitt) (administration secretary)))
    (assert! (salary (Aull DeWitt) 25000))
    (assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))
    
    (assert! (can-do-job (computer wizard) (computer programmer)))
    (assert! (can-do-job (computer wizard) (computer technician)))
    (assert! (can-do-job (administration secretary) (administration big wheel)))
    ))

(query-driver-loop-for-script
 '((assert! (rule (lives-near ?person-1 ?person-2)
                  (and (address ?person-1 (?town . ?rest-1))
                       (address ?person-2 (?town . ?rest-2))
                       (not (same ?person-1 ?person-2)))))
   (assert! (rule (same ?x ?x)))
   (assert! (rule (wheel ?person)
                  (and (supervisor ?middle-manager ?person)
                       (supervisor ?x ?middle-manager))))
   (assert! (rule (outranked-by ?staff-person ?boss)
                  (or (supervisor ?staff-person ?boss)
                      (and (supervisor ?staff-person ?middle-manager)
                           (outranked-by ?middle-manager ?boss)))))
   ))

(display "Simple queries")
(newline)(newline)
(query-driver-loop-for-script '(

    (job ?x (computer programmer))

    (address ?x ?y)

    (supervisor ?x ?x)

    (job ?x (computer ?type))

    (job ?x (computer . ?type))

    ))

(display "Compound queries")
(newline)(newline)
(query-driver-loop-for-script '(
                   
    (and (job ?person (computer programmer))
         (address ?person ?where))

    (or (supervisor ?x (Bitdiddle Ben))
        (supervisor ?x (Hacker Alyssa P)))
    
    (and (supervisor ?x (Bitdiddle Ben))
         (not (job ?x (computer programmer))))

    (and (salary ?person ?amount)
         (lisp-value > ?amount 30000))
    
    ))

(display "Rules")
(newline)(newline)
(query-driver-loop-for-script '(
                   
    (lives-near ?x (Bitdiddle Ben))

    (and (job ?x (computer programmer))
         (lives-near ?x (Bitdiddle Ben)))
    
    ))

(display "Exercise 4.55")
(newline)
(newline)

(query-driver-loop-for-script '(
                   
    ; a
    (supervisor ?person (Bitdiddle Ben))

    ; b
    (job ?name (accounting . ?type))

    ; c
    (address ?name (Slumerville . ?detail))

    ))


(display "Exercise 4.56")
(newline)
(newline)
(query-driver-loop-for-script '(

    ;a
    (and (supervisor ?person (Bitdiddle Ben))
         (address ?person ?where))

    ;b
    (and (salary ?person ?salary)
         (salary (Bitdiddle Ben) ?ben-salary)
         (lisp-value < ?salary ?ben-salary))

    ;c
    (and (supervisor ?name ?supervisor)
         (job ?supervisor ?supervisor-job)
         (not (job ?supervisor (computer . ?job-type)))
         )
    
    ))


(display "Exercise 4.57")
(newline)
(newline)
(query-driver-loop-for-script
 '((assert! (rule (can-replace ?person-1 ?person-2)
                  (and (or (and (job ?person-1 ?same-job)
                                (job ?person-2 ?same-job))
                           (and (can-do-job ?job-1 ?job-2)
                                (job ?person-1 ?job-1)
                                (job ?person-2 ?job-2)))
                       (not (same ?person-1 ?person-2)))))

   (can-replace ?who (Fect Cy D))

   (and (can-replace ?x ?y)
        (salary ?x ?sal-x)
        (salary ?y ?sal-y)
        (lisp-value < ?sal-x ?sal-y))
   
   ))


(display "Exercise 4.58")
(newline)
(newline)
(query-driver-loop-for-script
 '((assert! (rule (bigshot ?person)
                  (and (job ?person (?division . ?job-descr))
                       (or (not (supervisor ?person ?no-sup))
                           (and (supervisor ?person ?sup)
                                (not (job ?sup (?division . ?job-descr-sup))))))))

   (bigshot ?x)
   
   ))

(display "Exercise 4.59")
(newline)
(newline)
(query-driver-loop-for-script
 '((assert! (meeting accounting (Monday 9am)))
   (assert! (meeting administration (Monday 10am)))
   (assert! (meeting computer (Wednesday 3pm)))
   (assert! (meeting administration (Friday 1pm)))
   (assert! (meeting whole-company (Wednesday 4pm)))

   ;a
   (meeting ?department (Friday ?time))

   ;b
   (assert! (rule (meeting-time ?person ?day-and-time)
                  (or (meeting whole-company ?day-and-time)
                      (and (meeting ?department ?day-and-time)
                           (job ?person (?department . ?job-descr))))))
   (meeting-time (Bitdiddle Ben) ?when)

   ;c
   (meeting-time (Hacker Alyssa P) (Wednesday ?time))
   (meeting-time (Hacker Alyssa P) (Friday ?time))
   
   ))

(display "Exercise 4.60")
(newline)
(newline)

(define (name<? person-1 person-2)
  (string<? (symbol->string person-1)
            (symbol->string person-2)))

(query-driver-loop-for-script
 '(
   
   (lives-near (Hacker Alyssa P) ?who)
   
   (lives-near ?person-1 ?person-2)

   (assert! (rule (lives-near-fixed ?person-1 ?person-2)
                  (and (address ?person-1 (?town . ?rest-1))
                       (address ?person-2 (?town . ?rest-2))
                       (lisp-value (lambda (name-1 name-2)
                                     (string<? (apply string-append 
                                                      (map (lambda (s) (string-append s " ")) 
                                                           (map symbol->string name-1)))
                                               (apply string-append 
                                                      (map (lambda (s) (string-append s " ")) 
                                                           (map symbol->string name-2)))))
                                   ?person-1 ?person-2))))

   
   (lives-near-fixed ?person-1 ?person-2)
   
   ))


(display "Logic as programs")
(newline)(newline)
(query-driver-loop-for-script
 '(
   (assert! (rule (append-to-form () ?y ?y)))
   (assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                  (append-to-form ?v ?y ?z)))

   (append-to-form (a b) (c d) ?z)

   (append-to-form (a b) ?y (a b c d))

   (append-to-form ?x ?y (a b c d))
   
   ))


(display "Exercise 4.61")
(newline)(newline)
(query-driver-loop-for-script
 '(
   (assert! (rule (?x next-to ?y in (?x ?y . ?u))))
   (assert! (rule (?x next-to ?y in (?v . ?z))
                  (?x next-to ?y in ?z)))

   (?x next-to ?y in (1 (2 3) 4))
   
   (?x next-to 1 in (2 1 3 1))
   
   ))


(display "Exercise 4.62")
(newline)(newline)
(query-driver-loop-for-script
 '(
   (assert! (rule (last-pair (?x) (?x))))
   (assert! (rule (last-pair (?v . ?z) ?x)
                  (last-pair ?z ?x)))

   (last-pair (3) ?x)

   (last-pair (1 2 3) ?x)

   (last-pair (2 ?x) (3))

   ; goes into an infinite loop
   ;(last-pair ?x (3))
   
   ))


(display "Exercise 4.63")
(newline)(newline)
(query-driver-loop-for-script
 '(
   (assert! (son Adam Cain))
   (assert! (son Cain Enoch))
   (assert! (son Enoch Irad))
   (assert! (son Irad Mehujael))
   (assert! (son Mehujael Methushael))
   (assert! (son Methushael Lamech))
   (assert! (wife Lamech Ada))
   (assert! (son Ada Jabal))
   (assert! (son Ada Jubal))

   (assert! (rule (grandson ?g ?s)
                  (and (son ?g ?f)
                       (son ?f ?s))))

   (assert! (rule (son ?f ?s)
                  (and (wife ?f ?m)
                       (son ?m ?s))))

   (grandson Cain ?x)

   (son Lamech ?x)

   (grandson Methushael ?x)
   
   ))


(display "Exercise 4.64")
(newline)(newline)
(display "Infinite loop because ?middle-manager is unbound in one branch of the computation")
(newline)(newline)

(query-driver-loop-for-script-loop-detect
 '(
   
   (assert! (rule (outranked-by-v2 ?staff-person ?boss)
                  (or (supervisor ?staff-person ?boss)
                      (and (outranked-by-v2 ?middle-manager ?boss)
                           (supervisor ?staff-person ?middle-manager)))))
   
   (outranked-by-v2 (Bitdiddle Ben) ?who)

   ))


 
(display "Exercise 4.65")
(newline)(newline)
(display "Each result has a different ?middle-manager.")
(newline)(newline)
(query-driver-loop-for-script
 '(
   
   (wheel ?who)
   
   ))


(display "Exercise 4.67")
(newline)(newline)
(display "The following query would've failed without loop detection.")
(newline)(newline)
(query-driver-loop-for-script-loop-detect
 '(
   
   (assert! (married Minnie Mickey))

   (assert! (rule (married ?x ?y)
                  (married ?y ?x)))

   (married Mickey ?who)
   
   ))


(display "Exercise 4.68")
(newline)(newline)
(query-driver-loop-for-script-loop-detect
 '(
 
   (assert! (rule (append-to-form () ?y ?y)))
   (assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                  (append-to-form ?v ?y ?z)))
   
   (assert! (rule (reverse () ())))
   
   (assert! (rule (reverse (?x1 . ?xrest) ?y)
                  (and (reverse ?xrest ?y-except-last)
                       (append-to-form ?y-except-last (?x1) ?y))))

   (reverse (1 2 3) ?x)
   
   (reverse ?x (1 2 3))
   
   ))

#|
(display "Trying again with reverse rule added. Goes into infinite loop for both queries.")
(newline)(newline)
(query-driver-loop-for-script
 '(
   
   (assert! (rule (reverse ?x ?y)
                  (reverse ?y ?x)))

   (reverse (1 2 3) ?x)
   
   (reverse ?x (1 2 3))
   ))
|#

(display "Exercise 4.69")
(newline)(newline)
(query-driver-loop-for-script
 '(

   (assert! (rule ((grandson) ?g ?s) (grandson ?g ?s)))
   (assert! (rule ((great . ?rel) ?gg ?s)
                  (and (son ?f ?s)
                       (?rel ?gg ?f)
                       (last-pair ?rel (grandson)))))

   ((great grandson) ?g ?ggs)

   (?relationship Adam Irad)
   
   ))



(display "Exercise 4.71, 4.72, 4.73\n\n")
(display "These all have to do with how we handle infinite streams. Explicit delays and interleaving the streams all serve to ensure that initial and intermediate solutions are computed and displayed before continuing down the chain of infinite possibilities. You can find some nice examples at this link - https://www.inchmeal.io/sicp/ch-4/ex-4.71.html \n")
(query-driver-loop-for-script-loop-detect
 '(
   (assert! (ones ()))
   (assert! (rule (ones (1 . ?x)) (ones ?x)))

   (assert! (twos ()))
   (assert! (rule (twos (2 . ?x)) (twos ?x)))

   ))

(query-driver-loop-for-script-loop-detect '((ones ?x)))

(query-driver-loop-for-script-loop-detect '((or (ones ?x) (twos ?y))))

(query-driver-loop-for-script-loop-detect '((and (ones ?x) (twos ?y))))


(display "Exercise 4.74\n\n")
(display "Behaviour is unchanged with this modification. The implementation simply applies stream-car to the input stream after filtering out the null streams.\n\n\n")


(display "Exercise 4.75\n\n")
(query-driver-loop-for-script
 '(
   
   (unique (job ?x (computer wizard)))
   
   (unique (job ?x (computer programmer)))

   (and (job ?x ?j) (unique (job ?anyone ?j)))

   (and (job ?supervisor ?supervisor-job)
        (unique (supervisor ?x ?supervisor)))

   ))

