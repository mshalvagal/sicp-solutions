#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((incorrect-attempts 0))
    (define (dispatch key m)
      (if (eq? key password)
          (begin (set! incorrect-attempts 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT"
                                    m))))
          (begin (set! incorrect-attempts (+ incorrect-attempts 1))
                 (lambda (m)
                   (if (>= incorrect-attempts 3)
                       (begin
                         (display "Too many wrong password entries. The FBI is on their way. We kindly request you to cooperate.")
                         (newline))
                       (begin
                         (display "Incorrect password. Remaining attempts: ")
                         (display (- 2 incorrect-attempts))
                         (newline)))))))
    dispatch))

(define (make-joint acc og-password new-password)
  (let ((incorrect-attempts 0))
    (define (dispatch key m)
      (if (eq? key new-password)
          (begin (set! incorrect-attempts 0)
                 (acc og-password m))
          (begin (set! incorrect-attempts (+ incorrect-attempts 1))
                 (lambda (m)
                   (if (>= incorrect-attempts 3)
                       (begin
                         (display "Too many wrong password entries. The FBI is on their way. We kindly request you to cooperate.")
                         (newline))
                       (begin
                         (display "Incorrect password. Remaining attempts: ")
                         (display (- 2 incorrect-attempts))
                         (newline)))))))
    dispatch))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 40)
((peter-acc 'open-sesame 'deposit) 50)
((peter-acc 'open-sesame 'deposit) 50)
((paul-acc 'rosebud 'withdraw) 20)