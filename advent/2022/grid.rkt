#lang racket

(provide (all-defined-out))

(struct point (x y))

(struct grid (points width height) #:transparent)

(define (create-grid lines)
  (define cls (map string->list lines))
  (grid (list->vector
	  (map parse-number (apply append cls)))
	(length (first cls))
	(length cls)))

(define (parse-number c)
  (match c
    [#\1 1] [#\2 2] [#\3 3] [#\4 4] [#\5 5]
    [#\6 6] [#\7 7] [#\8 8] [#\9 9] [#\0 0]))

(define (get-point g x y)
  "returns values at specified point"
  (and (>= x 0)
       (< x (grid-width g))
       (>= y 0)
       (< y (grid-height g))
       (vector-ref (grid-points g) (+ (* (grid-width g) y) x))))

(define (telescope g x y step-x step-y [end? (lambda (v) (not v))])
  "collect all points in line until end?"
  (define pt (get-point g x y))
  (if (not pt)
    '()
    (cons pt 
	  (telescope g (+ x step-x) (+ y step-y) step-x step-y end?))))

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
)