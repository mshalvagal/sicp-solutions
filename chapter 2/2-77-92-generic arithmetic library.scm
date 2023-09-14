#lang sicp
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
    (if (and (not (eq? og-type drop-type)) (equ? re-raised arg))
         (drop dropped)
        arg)))


(define (raise-args target-type remaining-args result)
  (if (null? remaining-args)
      result
      (let* ((arg (car remaining-args))
             (type (type-tag arg)))
        (if (eq? type target-type)
            (raise-args target-type
                        (cdr remaining-args)
                        (append result (list arg)))
            (raise-args target-type
                        (cdr remaining-args)
                        (append result (list (raise-loop arg target-type))))))))

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
    (if (in-tower? result)
        (drop result)
        result)))

(define (in-tower? x)
  (and (pair? x) (memq (type-tag x) coercion-tower)))
(define (is-lower? value type)
  (let ((type-and-higher (memq type coercion-tower)))
    (if (and type-and-higher (in-tower? value))
        (not (memq (type-tag value) type-and-higher))
        (error "Reference type or provided value not in tower -- IS-LOWER?" (list type value)))))
(define (attach-tag type-tag contents) 
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum) 
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" (datum))))
(define (contents datum)
  (if (pair? datum) 
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" (datum))))

;;complex numbers - polar and rectangular packages
(define (install-rectangular-package)
  ;;internal procedures
  (define (r-part z) (car z))
  (define (i-part z) (cdr z))
  (define (make-from-real-imag x y)
    (if (and (is-lower? x 'complex) (is-lower? y 'complex))
        (cons (drop x) (drop y))
        (error "Got non-tagged real/imaginary parts -- RECTANGULAR PACKAGE" (list x y))))
  (define (magnitude z) 
    (square-root (add (square (r-part z))
                      (square (i-part z)))))
  (define (angle z) (arctan (i-part z)
                            (r-part z)))
  (define (make-from-mag-ang r a)
    (if (and (is-lower? r 'complex) (is-lower? a 'complex))
        (cons (mul r (cosine a)) (mul r (sine a)))
        (error "Got non-tagged magnitude/angle -- RECTANGULAR PACKAGE" (list r a))))
  ;;external interfaces
  (define (tag x) (attach-tag 'rectangular x))
  (put 'r-part '(rectangular) r-part)
  (put 'i-part '(rectangular) i-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'equ? '(rectangular rectangular)
       (lambda (x y) (and (equ? (r-part x) (r-part y))
                          (equ? (i-part x) (i-part y)))))
  (put '=zero? '(rectangular rectangular)
       (lambda (x y) (and (=zero? (r-part x))
                          (=zero? (i-part x)))))
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
    (if (and (is-lower? r 'complex) (is-lower? a 'complex))
        (cons (drop r) (drop a))
        (error "Got non-tagged magnitude/angle -- POLAR PACKAGE" (list r a))))
  (define (r-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (i-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (if (and (is-lower? x 'complex) (is-lower? y 'complex))
        (cons (square-root (add (square x) (square y)))
              (arctan y x))
        (error "Got non-tagged real/imaginary parts -- POLAR PACKAGE" (list x y))))
  ;;external interfaces
  (define (tag x) (attach-tag 'polar x))
  (put 'r-part '(polar) r-part)
  (put 'i-part '(polar) i-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ? '(polar polar)
       (lambda (x y) (and (equ? (magnitude x) (magnitude y))
                          (equ? (angle x) (angle y)))))
  (put '=zero? '(polar) (lambda (x) (=zero? (magnitude x))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)
(define (r-part z) (apply-generic 'r-part z))
(define (i-part z) (apply-generic 'i-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;Generic arithmetics package
(define (add x y) (apply-generic 'add x y))
(define (addd x y z) (apply-generic 'addd x y z))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (greater? x y) (apply-generic 'greater? x y))
(define (lesser? x y) (apply-generic 'lesser? x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (square x) (apply-generic 'mul x x))
(define (square-root x) (apply-generic 'square-root x))
(define (arctan x) (apply-generic 'arctan x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (define (int2rat x) (make-rational x 1))
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer) (lambda (x y) (= x y)))
  (put 'lesser? '(integer integer) (lambda (x y) (< x y)))
  (put 'greater? '(integer integer) (lambda (x y) (> x y)))
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put 'square-root '(integer) (lambda (x) (square-root (make-real x))))
  (put 'arctan '(integer integer) (lambda (x y) (arctan (make-real x) (make-real y))))
  (put 'sine '(integer) (lambda (x) (sine (make-real x))))
  (put 'cosine '(integer) (lambda (x) (cosine (make-real x))))
  (put 'addd '(integer integer integer) (lambda (x y z) (tag (+ x y z))))
  (put-coercion 'integer 'rational int2rat)
  'done)
(define (make-integer n)
  (if (integer? n)
      ((get 'make 'integer) n)
      (error "Got non-integer -- MAKE-INTEGER" n)))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (sqrt-real x)
    (let ((root (sqrt x)))
      (make-from-real-imag (tag (real-part root))
                           (tag (imag-part root)))))
  (define (real2complex x) (make-from-real-imag (tag x) (tag 0)))
  (define (real2rat x) 
    (make-rational (inexact->exact (numerator x))
                   (inexact->exact (denominator x))))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'lesser? '(real real) (lambda (x y) (< x y)))
  (put 'greater? '(real real) (lambda (x y) (> x y)))
  (put 'square-root '(real) (lambda (x) (sqrt-real x)))
  (put 'arctan '(real real) (lambda (x y) (tag (atan x y))))
  (put 'sine '(real) (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'addd '(real real real) (lambda (x y z) (tag (+ x y z))))
  (put-coercion 'real 'complex real2complex)
  (put-coercion 'real 'rational real2rat)
  (put 'make 'real (lambda (x) (tag x)))
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
  (define (rat2real x)
    (make-real (/ (numer x) (denom x))))
  (define (rat2int x) (make-integer (round (/ (numer x) (denom x)))))
  (put 'square-root '(rational) (lambda (x) (square-root (rat2real x))))
  (put 'arctan '(rational rational) (lambda (x y) (arctan (rat2real x) (rat2real y))))
  (put 'sine '(rational) (lambda (x) (sine (rat2real x))))
  (put 'cosine '(rational) (lambda (x) (cosine (rat2real x))))
  ;;external interfaces
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put 'lesser? '(rational rational) (lambda (x y) (lesser? (rat2real x) (rat2real y))))
  (put 'greater? '(rational rational) (lambda (x y) (greater? (rat2real x) (rat2real y))))
  (put-coercion 'rational 'real rat2real)
  (put-coercion 'rational 'integer rat2int)
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
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
  (define (add-complex z1 z2)
    (make-from-real-imag (add (r-part z1) (r-part z2))
                         (add (i-part z1) (i-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (r-part z1) (r-part z2))
                         (sub (i-part z1) (i-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (addd-complex z1 z2 z3)
    (make-from-real-imag (addd (r-part z1)
                               (r-part z2)
                               (r-part z3))
                         (addd (i-part z1)
                               (i-part z2)
                               (i-part z3))))
  (define (complex2real x)
    (let* ((r (r-part x))
           (type (type-tag r)))
      (cond ((eq? type 'real) r)
            ((is-lower? r 'real) (raise-loop r 'real))
            (else (error "Unable to drop complex to real -- COMPLEX2REAL" x)))))
  (define (complex2poly x)
    (make-from-coeffs 'unbound (list (drop (tag x)))))
  ;;external interfaces
  (define (tag x) (attach-tag 'complex x))
  (put-coercion 'complex 'real complex2real)
  (put-coercion 'complex 'polynomial complex2poly)
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'addd '(complex complex complex)
       (lambda (z1 z2 z3) (tag (addd-complex z1 z2 z3))))
  ;;these dispatch to the same procedure but the apply-generic
  ;;takes care of stripping away the type-tag at each level.
  (put 'r-part '(complex) r-part)
  (put 'i-part '(complex) i-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  'done)
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;coercion tower
(define coercion-tower '(integer rational real complex polynomial))

(define (raise-rec x types)
  (cond ((null? types) (error "Type not found in type tower -- RAISE-REC" (list x coercion-tower)))
        ((eq? (type-tag x) (car types))
         (if (null? (cdr types))
             x
             (let ((raiser (get-coercion (type-tag x) (cadr types))))
               (if raiser
                   (raiser (contents x))
                   x))))
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


(define (install-term-package)
  ;; internal procedures
  ;; representation of terms
  (define (make-term order coeff) (list order coeff))
  (define (order-term term) (car term))
  (define (coeff-term term) (cadr term))
  (define (equ-term? t1 t2)
    (and (equ? (order-term t1) (order-term t2))
         (equ? (coeff-term t1) (coeff-term t2))))
  ;; interface to rest of the system
  (define (tag t) (attach-tag 'term t))
  (put 'order '(term) order-term)
  (put 'coeff '(term) coeff-term)
  (put 'equ? '(term term) equ-term?)
  (put 'make 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  'done)
(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))


(define (install-sparse-term-package)
  ;; internal procedures
  (define (the-empty-termlist) '())
  (define (build-from-termlist terms result)
    (if (null? terms)
        result
        (build-from-termlist (cdr terms)
                             (insert-term (car terms) result))))
  (define (build-from-coeffs coeffs term-order)
    (cond ((null? coeffs) (the-empty-termlist))
          ((=zero? (car coeffs)) (build-from-coeffs (cdr coeffs)
                                                    (sub term-order (make-integer 1))))
          (else (cons (make-term term-order (car coeffs))
                      (build-from-coeffs (cdr coeffs)
                                         (sub term-order (make-integer 1)))))))
  (define (insert-term term terms)
    (if (empty-termlist? terms)
        (adjoin-term term (the-empty-termlist))
        (let* ((highest-term (first-term terms))
               (highest-order (order highest-term))
               (this-order (order term)))
          (cond ((greater? highest-order this-order)
                 (adjoin-term highest-term (insert-term term (rest-terms terms))))
                ((lesser? highest-order this-order)
                 (adjoin-term term terms))
                (else (adjoin-term (make-term this-order (add (coeff term) (coeff highest-term)))
                                   (rest-terms terms)))))))
  ;; representation of term lists
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((or (empty-termlist? term-list)
               (greater? (order term) (order (first-term term-list))))
           (cons term term-list))
          (else (error "Cannot adjoin term of lower order than term list (use insert-term instead) -- ADJOIN-TERM"
                       (list term term-list)))))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  ;; coercion
  (define (sparse2dense L)
    ((get 'make-from-termlist 'dense-termlist) L))
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'sparse-termlist L))
  (put-coercion 'sparse-termlist 'dense-termlist sparse2dense)
  (put 'first-term '(sparse-termlist) first-term)
  (put 'rest-terms '(sparse-termlist)
       (lambda (L) (tag (rest-terms L))))
  (put 'adjoin-term 'sparse-termlist
       (lambda (term L) (tag (adjoin-term term L))))
  (put 'the-empty-termlist 'sparse-termlist
       (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(sparse-termlist)
       (lambda (L) (empty-termlist? L)))
  (put 'make-from-termlist 'sparse-termlist
       (lambda (terms) (tag (build-from-termlist terms (the-empty-termlist)))))
  (put 'make-from-coeffs 'sparse-termlist
       (lambda (coeffs) (tag (build-from-coeffs coeffs
                                                (sub (make-integer (length coeffs))
                                                     (make-integer 1))))))
  'done)

(define (install-dense-term-package)
  ;; internal procedures
  (define (list-order term-list)
    (make-integer (- (length term-list) 1)))
  (define (strip-leading-zeros terms)
    (cond ((empty-termlist? terms) (the-empty-termlist))
          ((not (=zero? (first-term terms))) terms)
          (else (rest-terms terms))))
  (define (build-from-termlist terms result)
    (if (null? terms)
        result
        (build-from-termlist (cdr terms)
                             (insert-term (car terms) result))))
  (define (insert-term term terms)
    (if (empty-termlist? terms)
        (adjoin-term term (the-empty-termlist))
        (let* ((highest-term (first-term terms))
               (highest-order (order highest-term))
               (this-order (order term)))
          (cond ((greater? highest-order this-order)
                 (adjoin-term highest-term (insert-term term (rest-terms terms))))
                ((lesser? highest-order this-order)
                 (adjoin-term term terms))
                (else (adjoin-term (make-term this-order (add (coeff term) (coeff highest-term)))
                                   (rest-terms terms)))))))
  ;; representation of term lists
  (define (adjoin-term term term-list)
    (let ((term-order (order term))
          (term-coeff (coeff term)))
      (cond ((=zero? term-coeff) term-list)
            ((equ? term-order (add (make-integer 1) (list-order term-list)))
             (cons term-coeff term-list))
            ((greater? term-order (list-order term-list))
             (adjoin-term term (cons (make-integer 0) term-list)))
            (else (error "Cannot adjoin term of lower order than term list -- ADJOIN-TERM"
                         (list term term-list))))))
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list)
    (cond ((empty-termlist? term-list)
           (make-term (make-integer 0) (make-integer 0)))
          ((=zero? (car term-list))
           (first-term (cdr term-list)))
          (else (make-term (list-order term-list)
                           (car term-list)))))
  (define (rest-terms term-list)
    (let ((tail (cdr term-list)))
      (cond ((empty-termlist? tail) tail)
            ((=zero? (car tail)) (rest-terms tail))
            (else tail))))
  ;; coercion
  (define (dense2sparse L)
    ((get 'make-from-coeffs 'sparse-termlist) L))
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'dense-termlist L))
  (put 'first-term '(dense-termlist) first-term)
  (put 'rest-terms '(dense-termlist)
       (lambda (L) (tag (rest-terms L))))
  (put 'adjoin-term 'dense-termlist
       (lambda (term L) (tag (adjoin-term term L))))
  (put 'the-empty-termlist 'dense-termlist
       (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(dense-termlist) empty-termlist?)
  (put 'make-from-termlist 'dense-termlist
       (lambda (terms) (tag (build-from-termlist terms (the-empty-termlist)))))
  (put 'make-from-coeffs 'dense-termlist (lambda (coeffs) (tag coeffs)))
  (put-coercion 'dense-termlist 'sparse-termlist dense2sparse)
  'done)

(install-term-package)
(install-sparse-term-package)
(install-dense-term-package)
(define (constant-term x) (apply-generic 'constant-term x))

;;Polynomial library
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly var termlist)
    (cons var termlist))
  (define (make-from-termlist var terms)
    (make-poly var ((get 'make-from-termlist 'sparse-termlist) terms)))
  (define (make-from-coeffs var terms)
    (make-poly var ((get 'make-from-coeffs 'dense-termlist) terms)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x1 x2)
    (and (variable? x1) (variable? x2)
         (or (eq? x1 x2)
             (eq? x1 'unbound)
             (eq? x2 'unbound))))
  (define (select-variable p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (eq? v1 'unbound) v2 v1)))
  ;; term-list operations
  (define (first-term L)
    (apply-generic 'first-term L))
  (define (rest-terms L)
    (apply-generic 'rest-terms L))
  (define (empty-termlist? L)
    (apply-generic 'empty-termlist? L))
  (define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term-list)) term (contents term-list)))
  (define (the-empty-termlist)
    ((get 'the-empty-termlist 'sparse-termlist)))
  ;; arithmetic ops on termlists
  (define (negate-terms L)
    (mul-term-by-all-terms (make-term (make-integer 0)
                                      (make-integer -1)) L))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((greater? (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((lesser? (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1)
                                            (add (coeff t1) (coeff t2)))
                                 (add-terms (rest-terms L1) (rest-terms L2)))))))))
  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (add (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let* ((t1 (first-term L1))
               (t2 (first-term L2))
               (order1 (order t1))
               (order2 (order t2)))
          (if (greater? order2 order1)
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (coeff t1) (coeff t2)))
                     (new-o (sub order1 order2))
                     (new-term (make-term new-o new-c))
                     (new-term-neg (make-term new-o (mul new-c (make-integer -1)))))
                (let* ((new-dividend (add-terms L1
                                                (mul-term-by-all-terms new-term-neg L2)))
                       (rest-of-result (div-terms new-dividend L2))
                       (quotient (car rest-of-result))
                       (remainder (cadr rest-of-result)))
                  (list (adjoin-term new-term quotient) remainder)))))))
  (define (=zero-all-terms? L)
    (cond ((empty-termlist? L) #t)
          ((not (=zero? (coeff (first-term L)))) #f)
          (else (=zero-all-terms? (rest-terms L)))))
  (define (constant-term L)
    (cond ((empty-termlist? L) (make-integer 0))
          ((=zero? (order (first-term L))) (coeff (first-term L)))
          (else (constant-term (rest-terms L)))))
  ;; polynomial wrappers for arithmetic ops
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (select-variable p1 p2)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials do not correspond to the same variable -- ADD-POLY" (list p1 p2))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (select-variable p1 p2)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials do not correspond to the same variable -- SUB-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (select-variable p1 p2)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomials do not correspond to the same variable -- MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((var (select-variable p1 p2))
              (result (div-terms (term-list p1) (term-list p2))))
          (list (make-poly var (car result))
                (make-poly var (cadr result))))
        (error "Polynomials do not correspond to the same variable -- DIV-POLY" (list p1 p2))))
  (define (poly2complex p)
    (let ((constant (constant-term (term-list p))))
      (if (is-lower? constant 'complex)
          (raise-loop constant 'complex)
          constant)))
  ;; coercion
  (define (count-zero-coeffs L result)
    (cond ((empty-termlist? L) result)
          ((empty-termlist? (rest-terms L))
           (add result (order (first-term L))))
          (else (let ((term-order (order (first-term L)))
                      (next-order (order (first-term (rest-terms L)))))
                  (count-zero-coeffs (rest-terms L)
                                     (add (sub (sub term-order next-order)
                                               (make-integer 1))
                                          result))))))
  (define (best-representation num-terms num-zero-terms)
    (if (lesser? (div num-terms (add num-zero-terms (make-real 0.01)))
                 (make-integer 3))
        'sparse-termlist
        'dense-termlist))
  (define (to-best-representation L)
    (if (empty-termlist? L)
        L
        (let ((current-repr (type-tag L))
              (desired-repr (best-representation (add (order (first-term L)) (make-integer 1))
                                                 (count-zero-coeffs L (make-integer 0)))))
          (if (eq? current-repr desired-repr)
              L
              (let ((coercer (get-coercion current-repr desired-repr)))
                (if coercer
                    (coercer (contents L))
                    (error "Missing coercion -- TO-BEST-REPRESENTATION" (list current-repr desired-repr))))))))
  ;; Variable coercion
  (define (select-principal-variable v1 v2)
    (cond ((eq? v1 'unbound) v2)
          ((eq? v2 'unbound) v1)
          (else (let ((s1 (symbol->string v1))
                      (s2 (symbol->string v2)))
                  (if (string<=? s1 s2)
                      v1
                      v2)))))
  (define (express-in principal-variable p)
    (cond ((eq? principal-variable (variable p)) p)
          ((eq? 'unbound (variable p)) (make-poly principal-variable (term-list p)))
          (else (make-from-coeffs principal-variable (list (tag p))))))
  (define (add-simple p1 p2)
    (coerce-and-call p1 p2 add-poly))
  (define (residual-poly p)
    (let ((var (variable p))
          (terms (term-list p)))
      (if (empty-termlist? terms)
          (make-from-coeffs var (list (make-integer 0)))
          (make-poly var (rest-terms terms)))))
  (define (make-new-term-list var var-order L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (adjoin-term (make-term (order (first-term L))
                                (tag (express-in-principal-order (make-from-termlist var
                                                                                     (list (make-term var-order
                                                                                                      (coeff (first-term L))))))))
                     (make-new-term-list var var-order (rest-terms L)))))
  (define (exchange-or-combine-variables var var-order inner-poly)
    (let ((inner-var (variable inner-poly)))
      (if (same-variable? inner-var var)
          (mul-poly inner-poly
                    (make-from-termlist var (list (make-term var-order (make-integer 1)))))
          (make-poly inner-var
                     (make-new-term-list var var-order (term-list inner-poly))))))

  (define (express-in-principal-order p)
    (if (empty-termlist? (term-list p))
        (make-from-coeffs (variable p) (list (make-integer 0)))
        (let* ((term1 (first-term (term-list p)))
               (c1 (coeff term1))
               (o1 (order term1))
               (var (variable p)))
          (if (eq? (type-tag c1) 'polynomial)
              (let* ((c1-simpl (express-in-principal-order (contents c1)))
                     (principal-variable (select-principal-variable (variable c1-simpl) var)))
                (if (eq? principal-variable var)
                    (add-simple (make-from-termlist var (list (make-term o1 (tag c1-simpl))))
                                (express-in-principal-order (residual-poly p)))
                    (add-simple (exchange-or-combine-variables var o1 c1-simpl)
                                (express-in-principal-order (residual-poly p)))))
              (add-simple (make-from-termlist var (list (make-term o1 c1)))
                          (express-in-principal-order (residual-poly p)))))))

  
  (define (coerce-and-call p1 p2 op)
    (let* ((principal (select-principal-variable (variable p1) (variable p2)))
           (new-p1 (express-in principal p1))
           (new-p2 (express-in principal p2)))
      (op new-p1 new-p2)))
                                    
  ;; interface to rest of the system
  (define (tag p)
    (attach-tag 'polynomial
                (express-in-principal-order (make-poly (variable p)
                                                       (to-best-representation (term-list p))))))
  (put-coercion 'polynomial 'complex poly2complex)
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (coerce-and-call p1 p2 add-poly))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (coerce-and-call p1 p2 sub-poly))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (coerce-and-call p1 p2 mul-poly))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2)
         (let ((result (coerce-and-call p1 p2 div-poly)))
           (list (drop (tag (car result)))
                 (drop (tag (cadr result)))))))
  (put '=zero? '(polynomial) (lambda (x) (=zero-all-terms? (term-list x))))
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2)
         (and (same-variable? (variable p1) (variable p2))
              (=zero-all-terms? (sub-terms (term-list p1)
                                           (term-list p2))))))
  (put 'make-from-termlist 'polynomial
       (lambda (var terms) (tag (make-from-termlist var terms))))
  (put 'make-from-coeffs 'polynomial
       (lambda (var terms) (tag (make-from-coeffs var terms))))
  'done)
(define (make-from-termlist var terms)
  ((get 'make-from-termlist 'polynomial) var terms))
(define (make-from-coeffs var terms)
  ((get 'make-from-coeffs 'polynomial) var terms))


;;testing it all out
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)
(install-polynomial-package)


(newline)
(display "TESTING -- creating rational number 1/2") (newline)
(display (make-rational 1 2)) (newline)(newline)

(display "TESTING -- creating complex number (z1 = 3 + 4j) and magnitude 5") (newline)
(define z1 (make-from-real-imag (make-integer 3) (make-integer 4)))
(define z2 (make-from-real-imag (make-integer 3) (make-integer 4)))
(display z1) (newline)
(display (magnitude z1)) (newline)(newline)

(display "TESTING -- arithmetic ops (3 + 4), (3.0 + 4.0) and (z1 + z1)") (newline)
(display (add (make-integer 3) (make-integer 4))) (newline)
(display (add (make-real 3) (make-real 4))) (newline)
(display (add z1 z1)) (newline)(newline)

(display "TESTING -- equality op (z1 == z1), (1 == 1) and (1 == 2)") (newline)
(display (equ? z1 z2)) (newline)
(display (equ? (make-integer 1) (make-integer 1))) (newline)
(display (equ? (make-integer 1) (make-integer 2))) (newline)(newline)

(display "TESTING -- =zero? ops (1 - 1), (0.0*e^{5j}) and (1/2)") (newline)
(display (=zero? (sub (make-integer 1) (make-integer 1)))) (newline)
(display (=zero? (make-from-mag-ang (make-real 0) (make-real 5)))) (newline)
(display (=zero? (make-rational 1 2))) (newline)(newline)

(display "TESTING -- arithmetic ops with mixed args and coercion") (newline)
(display "TEST1 -- (3 + 4j) + 2.0") (newline)
(display (add z1 (make-real 2))) (newline)
(display "TEST2 -- (0.5 + 2j)") (newline)
(display (make-from-mag-ang (make-real 0.5) (make-integer 2)))(newline)
(display "TEST3 -- (3 + 4j) + 0.5") (newline)
(display (add z1 (make-real 0.5))) (newline)
(display "TEST4 -- (3 + 4j) - (3 + 4j)") (newline)
(display (sub z1 z1)) (newline) (newline)

(display "TESTING -- addition with three args (z1 + z1 + z1)") (newline)
(display (addd z1 z1 z1)) (newline) (newline)

(display "TESTING -- square root op (sqrt 4) (sqrt -1)") (newline)
(display (square-root (make-real 4)))(newline)
(display (square-root (make-integer -1)))(newline)(newline)

(display "TESTING -- sparse-and-dense-polynomial creation and arithmetic") (newline)
(define p1 (make-from-termlist 'x (list (make-term (make-integer 2) (make-real 3))
                                        (make-term (make-integer 0) (make-real 2.5)))))
(define p2 (make-from-termlist 'x (list (make-term (make-integer 1) (make-real 3.5))
                                        (make-term (make-integer 0) (make-real 4.5)))))
(define p4 (make-from-coeffs 'x (list (make-real 3) (make-integer 0) (make-real 2.5))))
(define p5 (make-from-coeffs 'x (list (make-real 3.5) (make-real 4.5))))
(display "TEST1 -- 0.0 + 4.5x^3") (newline)
(define p3 (make-from-termlist 'x (list (make-term (make-integer 0) (make-real 0.0))
                                        (make-term (make-integer 3) (make-real 4.5)))))
(define p6 (make-from-coeffs 'x (list (make-real 4.5) (make-integer 0) (make-integer 0) (make-real 0.0))))
(display p3) (newline)
(display p6) (newline)
(display "(p1 = 3x^2 + 2.5), (p2 = 3.5x + 4.5)") (newline)
(display "TEST2 -- p1 + p2") (newline)
(display (add p1 p2)) (newline)
(display (add p4 p5)) (newline)
(display "TEST3 -- p1 - p2") (newline)
(display (sub p1 p2)) (newline)
(display (sub p4 p5)) (newline)
(display "TEST4 -- p1 * p2") (newline)
(display (mul p1 p2)) (newline)
(display (mul p4 p5)) (newline) (newline)

(display "TEST5 -- (0x^0 == 0)") (newline)
(display (=zero? (make-from-termlist 'x '()))) (newline)
(display (=zero? (make-from-coeffs 'x '()))) (newline)
(display "TEST6 -- (0x^3 + (0/4)x^2 + 0x == 0)") (newline)
(display (=zero? (make-from-termlist 'x (list (make-term (make-integer 3) (make-real 0))
                                              (make-term (make-integer 2) (make-rational 0 4))
                                              (make-term (make-integer 1) (make-integer 0)))))) (newline)
(display (=zero? (make-from-coeffs 'x (list (make-real 0) (make-rational 0 4) (make-integer 0))))) (newline)
(display "TEST7 -- (0x^3 + (4/4)x^2 + 0x == 0)") (newline)
(display (=zero? (make-from-termlist 'x (list (make-term (make-integer 3) (make-real 0))
                                              (make-term (make-integer 2) (make-rational 4 4))
                                              (make-term (make-integer 1) (make-integer 0)))))) (newline)
(display (=zero? (make-from-coeffs 'x (list (make-real 0) (make-rational 4 4) (make-integer 0))))) (newline) (newline)

;; some nice examples to test copied from
;; http://jots-jottings.blogspot.com/2012/05/sicp-exercise-290-supporting-dense-and_9570.html
(display "TESTING -- polynomial ops from JOTS-JOTTINGS") (newline)
(define zero (make-integer 0))
(define dense
        (make-from-coeffs 'x (list (make-integer 4)
                                   (make-integer 3)
                                   (make-integer 2)
                                   (make-integer 1)
                                   zero)))
(define dense-with-many-zeros
        (make-from-coeffs 'x (list (make-integer 42)
                                   zero
                                   zero
                                   zero
                                   zero
                                   zero
                                   (make-integer -1))))

(define sparse
        (make-from-termlist 'x (list (make-term (make-integer 5) (make-integer 5))
                                     (make-term (make-integer 3) (make-integer 3))
                                     (make-term (make-integer 1) (make-integer 1)))))

(define another-sparse
        (make-from-termlist 'x (list (make-term (make-integer 5) (make-integer 5))
                                     (make-term (make-integer 3) (make-integer 3))
                                     (make-term (make-integer 1) (make-integer 1))
                                     (make-term (make-integer 0) (make-integer 3)))))

(define very-sparse
        (make-from-termlist 'x (list (make-term (make-integer 50) (make-integer 150))
                                     (make-term (make-integer 10) (make-integer 11))
                                     (make-term (make-integer 0) (make-integer 1)))))

(define polypoly
        (make-from-coeffs
         'x
         (list (make-from-coeffs 'y
                                 (list (make-integer 2)
                                       (make-integer 1))))))

(display (add polypoly dense)) (newline)
(display (add polypoly polypoly)) (newline)
(display (add (add polypoly polypoly) (make-integer 3))) (newline)
(display (add dense dense-with-many-zeros)) (newline)
(display (add dense-with-many-zeros dense-with-many-zeros)) (newline)
(display (add sparse sparse))(newline)
(display (add sparse another-sparse))(newline)
(display (add very-sparse sparse))(newline)
(display (mul sparse dense))(newline)
(display (sub sparse dense))(newline)
(display (sub (add dense (make-integer 1)) dense)) (newline) (newline)

;; some nice examples to test copied from
;; http://jots-jottings.blogspot.com/2012/06/sicp-exercise-291-dividing-polynomials.html
(display "TESTING -- polynomial division examples from JOTS-JOTTINGS") (newline)
(define sparse-numerator-1
  (make-from-termlist 'x
                      (list (make-term (make-integer 5) (make-integer 1))
                            (make-term (make-integer 0) (make-integer -1)))))

(define sparse-denominator-1
  (make-from-termlist 'x
                      (list (make-term (make-integer 2) (make-integer 1))
                            (make-term (make-integer 0) (make-integer -1)))))

(define sparse-numerator-2
  (make-from-termlist 'x
                      (list (make-term (make-integer 2) (make-integer 2))
                            (make-term (make-integer 0) (make-integer 2)))))

(define sparse-denominator-2
  (make-from-termlist 'x
                      (list (make-term (make-integer 2) (make-integer 1))
                            (make-term (make-integer 0) (make-integer 1)))))

(define sparse-numerator-3
  (make-from-termlist 'x
                      (list (make-term (make-integer 4) (make-integer 3))
                            (make-term (make-integer 3) (make-integer 7))
                            (make-term (make-integer 0) (make-integer 6)))))

(define sparse-denominator-3
  (make-from-termlist 'x
                      (list (make-term (make-integer 4) (make-real 0.5))
                            (make-term (make-integer 3) (make-integer 1))
                            (make-term (make-integer 0) (make-integer 3)))))

(define dense-numerator-1
  (make-from-coeffs 'x
                    (list (make-integer 1)
                          zero
                          zero
                          zero
                          zero
                          (make-integer -1))))

(define dense-denominator-1
  (make-from-coeffs 'x
                    (list (make-integer 1)
                          zero
                          (make-integer -1))))

(define dense-numerator-2
  (make-from-coeffs 'x
                    (list (make-integer 2)
                          zero
                          (make-integer 2))))

(define dense-denominator-2
  (make-from-coeffs 'x
                    (list (make-integer 1)
                          zero
                          (make-integer 1))))

(define dense-numerator-3
  (make-from-coeffs 'x
                    (list (make-integer 3)
                          (make-integer 7)
                          zero
                          zero
                          (make-integer 6))))

(define dense-denominator-3
  (make-from-coeffs 'x
                    (list (make-real 0.5)
                          (make-integer 1)
                          zero
                          zero
                          (make-integer 3))))

(display (div sparse-numerator-1 sparse-denominator-1)) (newline)
(display (div dense-numerator-1 dense-denominator-1)) (newline)
(display (div sparse-numerator-2 sparse-denominator-2)) (newline)
(display (div dense-numerator-2 dense-denominator-2)) (newline)
(display (div sparse-numerator-3 sparse-denominator-3)) (newline)
(display (div dense-numerator-3 dense-denominator-3)) (newline) (newline)


;; a nice example to test copied from
;; http://jots-jottings.blogspot.com/2012/06/sicp-exercise-292-dealing-with.html
(display "TESTING -- multiple variable polynomial simplification example from JOTS-JOTTINGS") (newline)
(define poly-1
  (make-from-coeffs 'x
                    (list (make-from-coeffs 'y
                                            (list (make-integer 5)
                                                  (make-integer 2)
                                                  (make-integer -1)))
                          (make-from-coeffs 'y
                                            (list (make-integer 2)
                                                  (make-integer 1)
                                                  (make-integer 2)))
                          (make-integer -3))))


(define poly-2
  (make-from-coeffs 'y
                    (list (make-from-coeffs 'x
                                            (list (make-integer 5)
                                                  (make-integer 2)
                                                  (make-integer 0)))
                          (make-from-coeffs 'x
                                            (list (make-integer 2)
                                                  (make-integer 1)
                                                  (make-integer 0)))
                          (make-from-coeffs 'x
                                            (list (make-integer -1)
                                                  (make-integer 2)
                                                  (make-integer -5))))))

(display poly-2) (newline) (newline)
(display "RESULT") (newline)
(display (sub poly-1 poly-2)) (newline) (newline)
