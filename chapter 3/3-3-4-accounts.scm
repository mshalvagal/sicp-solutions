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
                    (display (- 3 incorrect-attempts))
                    (newline)))))))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)