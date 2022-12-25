#lang racket

(provide (all-defined-out))

(struct point (x y) #:transparent)

(struct location point (z) #:transparent)

(struct grid (points width height) #:transparent)

(define (create-grid lines [converter parse-number])
  "creates a grid from input lines of characters"
  (define cls (map string->list lines))
  (grid (list->vector
	  (map converter (apply append cls)))
	(length (first cls))
	(length cls)))

(define (init-grid width height default-value)
  "creates a grid of a specified size all initialised to default-value"
  (grid (make-vector (* width height) default-value) width height))

(define (parse-number c)
  (match c
    [#\1 1] [#\2 2] [#\3 3] [#\4 4] [#\5 5]
    [#\6 6] [#\7 7] [#\8 8] [#\9 9] [#\0 0]))

(define (get-point g x y)
  "returns value at specified point"
  (and (>= x 0)
       (< x (grid-width g))
       (>= y 0)
       (< y (grid-height g))
       (vector-ref (grid-points g) (+ (* (grid-width g) y) x))))

(define (set-point g x y z)
  "sets the value at specified point"
  (define pos (+ (* (grid-width g) y) x))
  ; (displayln (format "setting ~v at pos ~v" z pos))
  (vector-set! (grid-points g) pos z)
  g)

(define (telescope g x y step-x step-y [end? (lambda (v) (not v))])
  "collect all points in line until end?"
  (define pt (get-point g x y))
  (if (not pt)
    '()
    (cons pt 
	  (telescope g (+ x step-x) (+ y step-y) step-x step-y end?))))

(define (get-cardinal-locations g x y)
  "collects all the cardinal locations around the x y position"
  (filter-map (lambda (p)
		(let ([z (get-point g (point-x p) (point-y p))])
		  (and z (location (point-x p) (point-y p) z))))
	      (list 
		(point x (- y 1)) ; up
		(point x (+ y 1)) ; down
		(point (- x 1) y) ; left
		(point (+ x 1) y)))) ; right

(define (draw-line x y end-x end-y)
  "creates a set of points between x/y and end"
  (if (and (equal? x end-x)
	   (equal? y end-y))
    (list (point x y))
    (cons (point x y)
	  (draw-line (add1-nearer x end-x)
		     (add1-nearer y end-y)
		     end-x
		     end-y))))

(define (add1-nearer n target)
  "moves n one step closer to target"
  (cond
    [(= n target) n]
    [(< n target) (add1 n)]
    [else (sub1 n)]))

(define (index-to-point g i)
  "converts the index of vector to a point"
  (point (modulo i (grid-width g))
	 (quotient i (grid-width g))))

(define (find-z-location g z)
  "finds location of the z value specified"
  (define p (index-to-point g (vector-member z (grid-points g))))
  (location (point-x p) (point-y p) z))

(define (draw-grid g)
  "draws the entire grids cells"
  (for-each 
    (lambda (row) 
      (for-each (lambda (column)
		  (display (get-point g column row)))
		(range 0 (grid-width g)))
      (displayln ""))
    (range 0 (grid-height g))))

(module+ test
  (require rackunit)

  (check-equal?
    (create-grid '("1234" "5678" "9012"))
    (grid (vector 1 2 3 4 5 6 7 8 9 0 1 2)
	  4 3))

  (define example
    (create-grid 
      (list 
	"30373"
	"25512"
	"65332"
	"33549"
	"35390")))

  (check-equal? (grid-width example) 5)
  (check-equal? (grid-height example) 5)

  (check-equal? (get-point example 1 1) 5)
  (check-equal? (get-point example 3 1) 1)
  (check-equal? (get-point example 2 2) 3)

  (check-equal? (find-z-location example 5) (location 1 1 5))
  (check-equal? (find-z-location example 9) (location 4 3 9))

  (check-equal? (draw-line 498 4 498 6)
		(list (point 498 4)
		      (point 498 5)
		      (point 498 6)))
  (check-equal? (draw-line 498 6 496 6)
		(list (point 498 6)
		      (point 497 6)
		      (point 496 6)))
)
