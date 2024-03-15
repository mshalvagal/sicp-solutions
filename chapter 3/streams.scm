#lang sicp
;stream implementations reproduced from https://www.lvguowei.me/post/sicp-goodness-stream-2/ (very nice blog)

(#%provide cons-stream)
(#%provide stream-car)
(#%provide stream-cdr)
(#%provide stream-ref)
(#%provide stream-for-each)
(#%provide stream-map)
(#%provide display-stream)
(#%provide display-line)
(#%provide finite-wrapper)
(#%provide stream-filter)
(#%provide add-streams)
(#%provide scale-stream)

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (memo-proc (lambda () exp)))))

(define (force delayed-object)
  (delayed-object))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (display x)
  (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define (finite-wrapper inf-stream n)
  (if (= n 0)
      '()
      (cons-stream (stream-car inf-stream)
                   (finite-wrapper (stream-cdr inf-stream)
                                   (- n 1)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))