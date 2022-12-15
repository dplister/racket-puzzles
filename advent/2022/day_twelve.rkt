#lang racket

(require "grid.rkt")

(define (create-map input)
  (create-grid input identity))

(define (get-starting-position g)
  (find-z-location g #\S))

(define (explore g explored)
  (define dirs 
    (filter (is-not-in explored)
	    (filter (is-above (first explored))
		    (get-cardinal-locations g 
					    (point-x (first explored)) 
					    (point-y (first explored))))))
  (cond 
    [(ormap is-end? dirs) (length explored)]
    [else
      (define further 
	(filter-map (lambda (d) (explore g (cons d explored))) dirs))
      (and (not (empty? further)) (apply min (flatten further)))]))

(define (is-end? l)
  (equal? (location-z l) #\E))

(define (is-not-in explored)
  (lambda (p) (not (member p explored))))

(define (is-above loc)
  "determines if the location is one above location"
  (define previous (if (equal? (location-z loc) #\S) #\a (location-z loc)))
  (lambda (v) 
    (let ([current (if (equal? (location-z v) #\E) #\z (location-z v))])
      (or (= (sub1 (char->integer current))
	   (char->integer previous))
	  (= (char->integer current)
	     (char->integer previous))))))

(define (get-input)
  (file->lines "day_twelve.txt"))

(define (part-a input)
  (define g (create-map input))
  (explore g (list (get-starting-position g))))

(part-a (get-input))

(module+ test
  (require rackunit)

  (check-equal? ((is-above (location 1 1 #\a)) (location 1 1 #\b)) #t)
  (check-equal? ((is-above (location 1 1 #\a)) (location 1 1 #\c)) #f)
  (check-equal? ((is-above (location 1 1 #\y)) (location 1 1 #\E)) #t)
  (check-equal? ((is-above (location 1 1 #\x)) (location 1 1 #\E)) #f)

  (check-equal? ((is-not-in (list (location 1 1 #\S))) (location 1 1 #\S)) #f)
  (check-equal? ((is-not-in (list (location 1 1 #\S))) (location 1 1 #\a)) #t)

  (define example 
    (list 
      "Sabqponm"
      "abcryxxl"
      "accszExk"
      "acctuvwj"
      "abdefghi"))

  (define example-grid (create-map example))

  (check-equal? (get-starting-position example-grid) (location 0 0 #\S))

  (check-equal? 
    (explore example-grid (list (get-starting-position example-grid)))
    31)

  (check-equal? 
    (part-a example)
    31)
)
