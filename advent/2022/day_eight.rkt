#lang racket

(require "../grid.rkt")

(define (get-point-set g dimension point-accessor)
  "retrieves the set of points across a grid dimension"
  (map (lambda (orientation) 
	 (sort (filter (lambda (p) (equal? (point-accessor p) orientation))
		 (all-points g))
	       (lambda (a b) 
		 (or (< (point-x a) (point-x b))
		     (and (< (point-x a) (point-x b))
			  (< (point-y a) (point-y b)))))))
       (range 0 (dimension g))))

(define (find-visible-trees g)
  (define columns (get-point-set g grid-width point-x))
  (define rows (get-point-set g grid-height point-y))
  (remove-duplicates
    (flatten 
      (append (map find-visible-treeline columns)
	      (map find-visible-treeline rows)))))

(define (find-visible-treeline trees)
  "finds all trees that are visible in line"
  (filter-map (lambda (i) 
		(and (or (is-visible-left (take trees (add1 i)))
			 (is-visible-right (drop trees i)))
		     (list-ref trees i)))
	  (range 0 (length trees))))

(define (print-trees ts)
  "outputs trees by their coordinates"
  (for-each
    (lambda (t) 
      (displayln 
	(format "X: ~v Y: ~v Z: ~v" (point-x t) (point-y t) (location-z t))))
    ts))

(define (is-visible-left ls)
  (or (empty? ls)
      (can-see-over 
	(location-z (last ls))
      	(rest (reverse (map location-z ls))))))

(define (is-visible-right ls)
  (or (empty? ls)
      (can-see-over 
	(location-z (first ls)) 
	(rest (map location-z ls)))))

(define (can-see-over v ls)
  "returns #t if v is higher than all in ls"
  (if (empty? ls) #t
    (and (> v (first ls))
	 (can-see-over v (rest ls)))))

(define (part-a)
  (length (find-visible-trees (parse-coords (file->lines "day_eight.txt") parse-number))))

(part-a)

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

  (check-equal? (find-visible-treeline (list (location 0 1 1) (location 0 1 2) (location 0 2 3)))
		(list (location 0 1 1) (location 0 1 2) (location 0 2 3)))

  (check-equal? (find-visible-treeline (list (location 0 0 3) (location 0 1 2) (location 0 1 2) (location 0 2 3)))
		(list (location 0 0 3) (location 0 2 3)))

  (check-equal? (length (find-visible-trees example)) 21)

)
