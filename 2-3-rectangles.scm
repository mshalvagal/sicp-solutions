(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment a b) (cons a b))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (sqr x) (* x x))

(define (segment-length seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (sqrt (+ (sqr (- (x-point p1) (x-point p2)))
             (sqr (- (y-point p1) (y-point p2)))))))

(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))


; Representation 1: corner points + angle between diagonal and width side

(define (make-rect p1 p2 angle-rect) (cons (make-segment p1 p2) angle-rect))
(define (diag-rect r) (car r))
(define (corner1-rect r) (start-segment (diag-rect r)))
(define (corner2-rect r) (end-segment (diag-rect r)))
(define (angle-diag-rect r) (cdr r))

(define (width-rect r) (* (segment-length (diag-rect r)) (cos (angle-diag-rect r))))
(define (height-rect r) (* (segment-length (diag-rect r)) (sin (angle-diag-rect r))))

(define corner1 (make-point 1 1))
(define corner2 (make-point 5 5))
(define angle1 0.2)

(define r1 (make-rect corner1 corner2 angle1))

(display "Rectangle 1: ") (newline)
(display "Perimeter: ") (display (perimeter-rect r1)) (newline)
(display "Area ") (display (area-rect r1)) (newline) (newline)


; Representation 2: base segment + height

(define (make-rect base height) (cons base height))
(define (base-rect r) (car r))
(define (width-rect r) (segment-length (base-rect r)))
(define (height-rect r) (cdr r))

(define p1 (make-point 1 1))
(define p2 (make-point 4 3))
(define height 3)
(define r1 (make-rect (make-segment p1 p2) height))

(display "Rectangle 2: ") (newline)
(display "Perimeter: ") (display (perimeter-rect r1)) (newline)
(display "Area ") (display (area-rect r1)) (newline) (newline)

; TODO - verify equivalence, needs some simple geometry to convert rectangles between formats that i'm too lazy to do right now
