#lang sicp
(#%require "meta-lisp-evaluator.scm")

;Exercise 4.11
(define (make-frame-alt variables values)
  (map (lambda (x y)
         (cons x y))
       variables
       values))
(define (frame-variables-alt frame) (map car frame))
(define (frame-values-alt frame) (map cdr frame))
(define (add-binding-to-frame-alt! var val frame)
  (set-car! frame (cons (cons var val)
                        frame)))

(define (lookup-variable-value-alt var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (frame-variable pair) (car pair))
(define (frame-variable-value pair) (cdr pair))

(define (set-variable-value-alt! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (frame-variable (car frame)))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame1 (first-frame env)))
          (scan frame1))))
  (env-loop env))

(define (define-variable-alt! var val env)
  (let ((frame1 (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame-alt! var val frame))
            ((eq? var (frame-variable (car frame)))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (scan frame1)))


;Exercise 4.12
(define (scan-frame var vars vals)
  (cond ((null? vars) '())
        ((eq? var (car vars)) (make-frame vars vals))
        (else (scan-frame (cdr vars) (cdr vals)))))

(define (scan-env var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let* ((frame (first-frame env))
             (scan-result (scan-frame var
                                      (frame-variables frame)
                                      (frame-values frame))))
        (if (null? scan-result)
            (scan-env (enclosing-environment env))))))

(define (lookup-variable-value-abstr var env)
  (let ((found-frame-list (scan-env var env)))
    (car (frame-values found-frame-list))))

(define (set-variable-value-abstr! var val env)
  (let ((found-frame-list (scan-env var env)))
    (set-car! (frame-values found-frame-list) val)))

(define (define-variable! var val env)
  (let* ((frame1 (first-frame env))
         (found-frame-list (scan-frame var
                                       (frame-variables frame1)
                                       (frame-values frame1))))
    (if (null? found-frame-list)
        (add-binding-to-frame! var val frame1)
        (set-car! (frame-values found-frame-list) val))))


;Exercise 4.13
(define (make-unbound! var env)
  (let* ((frame1 (first-frame env))
         (found-frame-list (scan-frame var
                                       (frame-variables frame1)
                                       (frame-values frame1))))
    (if found-frame-list
        (let ((vars (frame-variables found-frame-list))
              (vals (frame-values found-frame-list)))
          (set-car! vars (cdr vars))
          (set-car! vals (cdr vals))))))

  
  

