#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seq-of-seq)
  (if (null? seq-of-seq)
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seq-of-seq))
            (accumulate-n op init (if (null? (cdar seq-of-seq))
                                      '()
                                      (map (lambda (x) (cdr x)) seq-of-seq))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions k board-size))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? positions k board-size)
  (if (= k 1)
      #t
      (let ((forbidden-rows (filter
                             (lambda (x) (and (> x 0) (< x (+ board-size 1))))
                             (append
                              (cdr positions)
                              (accumulate-n + 0 (list (cdr positions) (enumerate-interval 1 (- k 1))))
                              (accumulate-n - 0 (list (cdr positions) (enumerate-interval 1 (- k 1)))))))
            (new-row (car positions)))
        (accumulate (lambda (x y)
                      (and (not (= x new-row)) y))
                    #t
                    forbidden-rows))))

(display (queens 3)) (newline)
(display (queens 4)) (newline)
(display (queens 5)) (newline)
(display (queens 6)) (newline)
(display (queens 8)) (newline)
