(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

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

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (valid-sum? triple s)
  (= (+ (car triple) (cadr triple) (cadr (cdr triple))) s))

(define (make-triple-sum triple)
  (list (car triple) 
        (cadr triple)
        (cadr (cdr triple))
        (+ (car triple) (cadr triple) (cadr (cdr triple)))))

(define (sum-triples n s)
  (map make-triple-sum
       (filter (lambda (x) (valid-sum? x s))
               (unique-triples n))))

(display (unique-triples 5)) (newline)
(display (sum-triples 5 10)) (newline)
