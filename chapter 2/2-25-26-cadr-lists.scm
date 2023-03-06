(define list-1 (list 1 3 (list 5 7) 9))
(define list-2 (list (list 7)))
(define list-3 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7)))))))
(define list-4 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7))))))))

(display (car (cdr (car (cdr (cdr list-1)))))) (newline)
(display (cadr (cadr (cdr list-1)))) (newline)
(display (car (car list-2))) (newline)
(display (caar list-2)) (newline)
(display (cdr (cdr (cdr (cdr (cdr (cdr list-3))))))) (newline)
(display (car (cadr (cadr (cadr (cadr (cadr (cadr list-4)))))))) (newline)

(define x (list 1 2 3))
(define y (list 4 5 6))

(display (append x y)) (newline) ;one null is removed from end of list 1
(display (cons x y)) (newline) ;cons treats list 1 as car and list 2 as cdr, but cdring gives now list 2 proper, so no new null is added
(display (list x y)) (newline) ;list adds a new terminal null and treats the lists as elements
