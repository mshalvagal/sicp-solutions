#lang sicp
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (count-pairs-correct x)
  (let ((record '()))
    (define (count x-cur)
      (if (and (pair? x-cur) (not (memq x-cur record)))
          (begin
            (set! record (cons x-cur record))
            (+ (count (car x-cur))
               (count (cdr x-cur))
               1))
          0))
    (count x)))

(define (contains-cycle? x)
  (let ((record '()))
    (define (test-for-cycle x-cur)
       (cond ((not (pair? x-cur)) #f)
             ((memq x-cur record) #t)
             (else (begin
                     (set! record (cons x-cur record))
                     (test-for-cycle (cdr x-cur))))))
     (test-for-cycle x)))

(define (contains-cycle2? x)
  (define (test-for-cycle hare turtle)
    (if (not (pair? (cdr hare)))
        #f
        (or (eq? hare turtle)
            (test-for-cycle (cddr hare)
                            (cdr turtle)))))
  (if (or (not (pair? x))
          (not (pair? (cdr x)))
          (not (pair? (cddr x))))
      #f
      (test-for-cycle (cddr x) (cdr x))))
          

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define list1 (list 'a 'b 'c))

(define x (list 'a))
(define list2 (list x x))

(define y (cons x x))
(define list3 (cons y y))

(define list4 (make-cycle (list 'a 'b 'c)))

(count-pairs list1)
(count-pairs list2)
(count-pairs list3)

(newline)

(count-pairs-correct list1)
(count-pairs-correct list2)
(count-pairs-correct list3)
(count-pairs-correct list4)

(newline)

(contains-cycle? list1)
(contains-cycle? list2)
(contains-cycle? list3)
(contains-cycle? list4)

(newline)

(contains-cycle2? list1)
(contains-cycle2? list2)
(contains-cycle2? list3)
(contains-cycle2? list4)