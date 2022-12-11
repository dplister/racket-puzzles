#lang racket

(require "grid.rkt")

(define (find-visible-trees g)
  (flatten
    (map 
      (lambda (y) 
	(filter-map 
	  (lambda (x) 
	    (and
	      (is-visible g x y)
	      (get-point g x y)))
	  (range 0 (grid-width g))))
      (range 0 (grid-height g)))))

(define (is-visible g x y)
  "determines if the current point is visible from the edge of the grid"
  (or (can-see-over g x y 0 -1) ; up
      (can-see-over g x y 1 0) ; right
      (can-see-over g x y -1 0) ; left
      (can-see-over g x y 0 1))) ; down

(define (can-see-over g x y step-x step-y)
  "returns set of trees that have a higher value than the previous"
  (define ps (telescope g x y step-x step-y))
  (andmap (lambda (p) (> (first ps) p)) (rest ps)))

(define (biggest-area g)
  (apply max
    (map 
      (lambda (y) 
	(apply max 
	       (map 
		 (lambda (x) 
		   (area g x y))
		 (range 0 (grid-width g)))))
      (range 0 (grid-height g)))))

(define (area g x y)
  "calculates the total trees visible in the cardinal directions"
  (* (visible-distance g x y 0 -1) ; up
     (visible-distance g x y 1 0) ; right
     (visible-distance g x y -1 0) ; left
     (visible-distance g x y 0 1))) ; down

(define (visible-distance g x y step-x step-y)
  (define ps (telescope g x y step-x step-y))
  (define (loop v ls)
    (cond
      [(empty? ls) 0]
      [(<= v (first ls)) 1]
      [else (+ 1 (loop v (rest ls)))]))
  (loop (first ps) (rest ps)))

(define (part-a)
  (length 
    (find-visible-trees 
      (create-grid (file->lines "day_eight.txt")))))

(part-a)

(define (part-b)
  (biggest-area
    (create-grid (file->lines "day_eight.txt"))))

(part-b)

(module+ test
  (require rackunit)

  (define example
    (create-grid
      (list "30373"
	    "25512"
	    "65332"
	    "33549"
	    "35390")))

  (check-equal? (length (find-visible-trees example)) 21)
  (check-equal? (biggest-area example) 8)

)
