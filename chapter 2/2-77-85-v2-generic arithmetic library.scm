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
(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))
(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))


;;helper functions
(define (raise-loop arg target-type)
  (let ((raised (raise arg)))
    (if (eq? (type-tag raised) target-type)
        raised
        (raise-loop raised target-type))))

(define (drop arg)
  (let* ((og-type (type-tag arg))
         (dropped (project arg))
         (drop-type (type-tag dropped))
         (re-raised (raise dropped)))
    (if (and (equ? re-raised arg) (not (eq? og-type drop-type)))
        (drop dropped)
        arg)))

(define (raise-args highest-type remaining-args result)
  (if (null? remaining-args)
      result
      (let* ((arg (car remaining-args))
             (type (type-tag arg)))
        (if (eq? type highest-type)
            (raise-args highest-type
                        (cdr remaining-args)
                        (append result (list arg)))
            (raise-args highest-type
                        (cdr remaining-args)
                        (append result (list (raise-loop arg highest-type))))))))

(define (apply-generic op . args)
  (define (search-and-apply-op)
    (let* ((type-tags (map type-tag args))
           (proc (get op type-tags)))
     (if proc
         (apply proc (map contents args))
         (let ((raised (raise-args (highest-type type-tags) args '())))
           (if raised
               (let* ((type-tags-raised (map type-tag raised))
                      (alternate-proc (get op type-tags-raised)))
                 (if alternate-proc
                     (apply alternate-proc (map contents raised))
                     (error "No suitable method found -- APPLY-GENERIC" (list op type-tags))))
               (error "No suitable coercions found -- APPLY-GENERIC"))))))
  (let ((result (search-and-apply-op)))
    (if (and (pair? result) (memq (type-tag result) coercion-tower))
        (drop result)
        result)))

(define (attach-tag type-tag contents) 
  (cons type-tag contents))
(define (type-tag datum)
  (cond ((integer? datum) 'integer)
        ((pair? datum) (car datum))
        (error "Bad tagged datum -- TYPE-TAG" (datum))))
(define (contents datum)
  (cond ((integer? datum) datum)
        ((pair? datum) (cdr datum))
        (error "Bad tagged datum -- CONTENTS" (datum))))
(define (square x) (* x x))


;;complex numbers - polar and rectangular packages
(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (if (and (real? x) (real? y))
        (cons x y)
        (error "Got non-real real/imaginary parts -- RECTANGULAR PACKAGE" (list x y))))
  (define (magnitude z) 
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z) (atan (imag-part z)
                          (real-part z)))
  (define (make-from-mag-ang r a)
    (if (and (real? r) (real? a))
        (cons (* r (cos a)) (* r (sin a)))
        (error "Got non-real magnitude/angle -- RECTANGULAR PACKAGE" (list r a))))
  ;;external interfaces
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a)
    (if (and (real? r) (real? a))
        (cons r a)
        (error "Got non-real magnitude/angle -- POLAR PACKAGE" (list r a))))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (if (and (real? x) (real? y))
        (cons (sqrt (+ (square x) (square y)))
              (atan y x))
        (error "Got non-real real/imaginary parts -- POLAR PACKAGE" (list x y))))
  ;;external interfaces
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;Generic arithmetics package
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-integer-package)
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  (put 'div '(integer integer) /)
  'done)
(define (make-integer n)
  (if (integer? n)
      n
      (error "Got non-integer -- MAKE-INTEGER" n)))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  'done)
(define (make-real x)
  ((get 'make 'real) x))

(define (install-rational-package)
  ;;internal functions
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (and (integer? n) (integer? d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (error "Got non-integer numerator/denominator -- RATIONAL PACKAGE" (list n d))))
  (define (add x y)
    (make-rat (+ (* (numer x) (denom y)) 
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  (define (sub x y)
    (make-rat (- (* (numer x) (denom y)) 
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  (define (mul x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;;external interfaces
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

(define (install-complex-package)
  ;;inherited functions from rectangular and polar
  (define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;;internal procedures
  (define (add z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (addd-complex z1 z2 z3)
    (make-from-real-imag (+ (real-part z1)
                            (real-part z2)
                            (real-part z3))
                         (+ (imag-part z1)
                            (imag-part z2)
                            (imag-part z3))))
  ;;external interfaces
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'addd '(complex complex complex)
       (lambda (z1 z2 z3) (tag (addd-complex z1 z2 z3))))
  ;;these dispatch to the same procedure but the apply-generic
  ;;takes care of stripping away the type-tag at each level.
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;this is technically bad practice to define package functionality
;;outside of the individual complex/rational packages because
;;now we need to know the internal structure of each implementation
;;and therefore the whole thing is brittle to changes in the definitions.
;;but i've done it here this way to demonstrate that additivity is 
;;possible but potentially messy with data-directed approaches
(define (install-auxiliary-package)
  (define (equal-pair? x y) (and (= (car x) (car y))
                                 (= (cdr x) (cdr y))))
  (define (=zero-rat? x) (= (car x) 0))
  (define (=zero-rectangular? x) (and (= (car x) 0)
                                      (= (cdr x) 0)))
  (define (=zero-polar? x) (= (car x)))
  ;;external interfaces
  (put 'equ? '(integer integer) (lambda (x y) (= x y)))
  (put 'equ? '(real real) =)
  (put 'equ? '(rational rational) equal-pair?)
  (put 'equ? '(rectangular rectangular) equal-pair?)
  (put 'equ? '(polar polar) equal-pair?)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put '=zero? '(rational) =zero-rat?)
  (put '=zero? '(rectangular) =zero-rectangular?)
  (put '=zero? '(polar) =zero-polar?)
  (put '=zero? '(complex) =zero?)
  'done)
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;;coercion tower
(define coercion-tower '(integer rational real complex))

(define (raise-rec x types)
  (cond ((null? types) (error "Type not found in type tower -- RAISE" (list x coercion-tower)))
        ((eq? (type-tag x) (car types))
         (if (null? (cdr types))
             x
             (let ((raiser (get-coercion (type-tag x) (cadr types))))
               (if raiser
                   (raiser x)
                   (error "Raising coercion not found for pairs -- RAISE" (list (type-tag x) (cadr types)))))))
        (else (raise-rec x (cdr types)))))

(define (raise x)
  (raise-rec x coercion-tower))

(define (project x)
  (raise-rec x (reverse coercion-tower)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (highest-type type-list)
  (define (filter-lowest-type remaining-tower remaining-types)
    (if (null? remaining-tower)
        (error "Some types were not in the tower -- HIGHEST-TYPE" (list type-list coercion-tower))
        (let* ((predicate (lambda (x) (not (eq? x (car remaining-tower)))))
               (filtered-types (filter predicate remaining-types)))
          (if (null? filtered-types)
              (car remaining-tower)
              (filter-lowest-type (cdr remaining-tower) filtered-types)))))
  (if (null? type-list)
      #f
      (filter-lowest-type coercion-tower type-list)))

(define (install-coercion-tower)
  (define (int2rat x) (make-rational x 1))
  (define (rat2real x) (make-real (/ (numer x) (denom x))))
  (define (real2complex x) (make-from-real-imag (contents x) 0))
  (define (rat2int x)
    (round (/ (numer x) (denom x))))
  (define (real2rat x) 
    (make-rational (inexact->exact (numerator (contents x)))
                   (inexact->exact (denominator (contents x)))))
  (define (complex2real x) (make-real (real-part x)))
  (put-coercion 'integer 'rational int2rat)
  (put-coercion 'rational 'real rat2real)
  (put-coercion 'real 'complex real2complex)
  (put-coercion 'rational 'integer rat2int)
  (put-coercion 'real 'rational real2rat)
  (put-coercion 'complex 'real complex2real))


;;testing it all out
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)
(install-auxiliary-package)
(install-coercion-tower)

(newline)
(display (make-rational 1 2)) (newline)
(display (make-from-real-imag 1 2)) (newline)(newline)

(define z1 (make-from-real-imag 3 4))
(define z2 (make-from-real-imag 3 4))
(display (magnitude z1)) (newline)(newline)

(display (add 3 4)) (newline)(newline)

(display (equ? z1 z2)) (newline)
(display (equ? 1 1)) (newline)
(display (equ? 1 2)) (newline)(newline)

(display (=zero? (- 1 1))) (newline)
(display (=zero? (make-from-mag-ang 0 5))) (newline)
(display (=zero? (make-rational 1 2))) (newline)(newline)

(display (add z1 2)) (newline)
(display (add z1 (make-real 0.5))) (newline) (newline)

(define (addd x y z) (apply-generic 'addd x y z))
(display (addd z1 z1 z1)) (newline) (newline)

(display (sub z1 z1)) (newline)
(display (make-real 0.5)) (newline)

