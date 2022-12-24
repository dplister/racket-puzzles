#lang racket

(require "grid.rkt")

(define (parse-lines inputs)
  "converts input set into a set of point sets"
  (define (loop ls)
    (if (<= (length ls) 1) (list)
      (append (apply draw-line (append (first ls) (second ls)))
	      (loop (rest ls)))))
  (flatten (map loop (map parse-line inputs))))

(define (parse-line input)
  "converts a line into a set of points"
  (map parse-point (string-split input " -> ")))

(define (parse-point input)
  "converts a point definition into a set of numbers"
  (map string->number (string-split input ",")))

(define (grid-from-points points)
  "creates a grid that fits in the set of points provided"
  (define offset-x (apply min (map point-x points)))
  (define offset-y (apply min (map point-y points)))
  (define max-x (apply max (map point-x points)))
  (define max-y (apply max (map point-y points)))
  (foldl 
    (lambda (p result) 
      (set-point 
	result 
	(- (point-x p) offset-x)
	(- (point-y p) offset-y)
	#\#))
    (init-grid (+ 1 (- max-x offset-x)) 
	       (+ 1 (- max-y offset-y)) 
	       #\.)
    points))

(module+ test
  (require rackunit)

  (check-equal? (parse-point "123,4") '(123 4))
  (check-equal? (parse-point "321,6") '(321 6))

  (define example
    (list "498,4 -> 498,6 -> 496,6"
	  "503,4 -> 502,4 -> 502,9 -> 494,9"))

  (check-equal? (parse-line (first example)) '((498 4) (498 6) (496 6)))
  (check-equal? (parse-line (second example)) '((503 4) (502 4) (502 9) (494 9)))

  (draw-grid (grid-from-points (parse-lines example)))
)
