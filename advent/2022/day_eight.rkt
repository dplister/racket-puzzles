#lang racket

(require "../grid.rkt")

(define (get-point-set g dimension point-accessor)
  "retrieves the set of points across a grid dimension"
  (map (lambda (orientation) 
	 (filter (lambda (p) (equal? (point-accessor p) orientation))
		 (all-points g)))
       (range 0 (dimension g))))

(define (find-visible-trees g)
  (define columns (get-point-set g grid-width point-x))
  (define rows (get-point-set g grid-height point-y))
  (remove-duplicates
    (flatten 
      (append 
	(map find-visible-treeline columns)
	(map find-visible-treeline rows)))))

(define (find-visible-treeline trees)
  "finds all trees that are visible in line"
  (filter-map (lambda (i) 
		(and (or (is-visible-left (take trees (add1 i)))
			 (is-visible-right (drop trees i)))
		     (list-ref trees i)))
	  (range 0 (length trees))))

(define (is-visible-left ls)
  (displayln (format "LEFT: ~v" ls))
  (or (empty? ls)
      (apply > (reverse (map location-z ls)))))

(define (is-visible-right ls)
  (displayln (format "RIGHT: ~v" ls))
  (or (empty? ls)
      (apply > (map location-z ls))))

(module+ test
  (require rackunit)

  (define example
    (parse-coords 
      (list "30373"
	    "25512"
	    "65332"
	    "33549"
	    "35390")
      parse-number))

;  (check-equal? (find-visible-treeline (list (location 0 1 1) (location 0 1 2) (location 0 2 3)))
;		(list (location 0 1 1) (location 0 1 2) (location 0 2 3)))

  ;(check-equal? (find-visible-treeline (list (location 0 0 3) (location 0 1 2) (location 0 1 2) (location 0 2 3)))
;		(list (location 0 0 3) (location 0 2 3)))

  ;(check-equal? (length (find-visible-trees example)) 21)

  (for-each (lambda (t) 
	      (displayln (format "X: ~v Y: ~v Z: ~v" (point-x t) (point-y t) (location-z t))))
	    (find-visible-trees example))
)
