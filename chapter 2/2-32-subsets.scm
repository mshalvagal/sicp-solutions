(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (subset) 
                       (append (list (car s)) subset)) 
                     rest)))))

(display (subsets (list 1 2 3)))
