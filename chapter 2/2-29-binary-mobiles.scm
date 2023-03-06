(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (is-balanced? x)
  (define (is-balanced-aux? mobile)
    (if (not (pair? mobile))
        (list #t mobile)
        (let ((left-length (branch-length (left-branch mobile)))
              (right-length (branch-length (right-branch mobile)))
              (left-result (is-balanced-aux? (branch-structure (left-branch mobile))))
              (right-result (is-balanced-aux? (branch-structure (right-branch mobile)))))
          (let ((is-balanced-left? (car left-result))
                (is-balanced-right? (car right-result))
                (left-weight (cadr left-result))
                (right-weight (cadr right-result)))
            (list (and is-balanced-left? 
                       is-balanced-right? 
                       (= (* left-weight left-length) (* right-weight right-length)))
                  (+ left-weight right-weight))))))
  (car (is-balanced-aux? x)))


(define b11 (make-branch 1 6))
(define b12 (make-branch 2 3))
(define s11 (make-mobile b11 b12))

(display (total-weight s11)) (newline)
(display (is-balanced? s11)) (newline)


(define b13 (make-branch 1 9))
(define b14 (make-branch 1 9))
(define s12 (make-mobile b13 b14))

(define b21 (make-branch 1 s11))
(define b22 (make-branch 0.5 s12))
(define s (make-mobile b21 b22))

(display (total-weight s)) (newline)
(display (is-balanced? s)) (newline)
