#lang racket

(define (first-range ls)
  "determines which range starts first, if both same, largest first"
  (let ([l1 (first ls)]
	[l2 (second ls)])
    (cond
      [(= (first l1) (first l2))
       (if (< (second l1) (second l2))
	 (list l2 l1)
	 (list l1 l2))]
      [(< (first l1) (first l2))
       (list l1 l2)]
      [else
	(list l2 l1)])))

(define (parse-range input)
  "converts string input into numerical range"
  (map string->number (string-split input "-")))

(define (parse-line input)
  "converts line into two ranges"
  (map parse-range (string-split input ",")))

(define (is-within-range ls)
  "determines if second list can fit within first"
  (let ([l1 (first ls)]
	[l2 (second ls)])
  (and (>= (first l2) (first l1))
       (<= (second l2) (second l1)))))

(define (get-input)
  (map parse-line (file->lines "day_four.txt")))

(define (count-valid-ranges ranges valid?)
  "counts the ranges that meet the validation criteria"
  (foldl (lambda (current result)
	   (if (valid? (first-range current))
	     (add1 result) result))
	 0 ranges))

(define (part-a)
  (count-valid-ranges (get-input) is-within-range))

(part-a)

(define (ranges-intersect ls)
  "determines if either range intersects"
  (let ([l1 (first ls)]
	[l2 (second ls)])
    (<= (first l2) (second l1))))

(define (part-b)
  (count-valid-ranges (get-input) ranges-intersect))

(part-b)

(module+ test
  (require rackunit)

  (check-equal? (first-range '((2 3) (3 4))) '((2 3) (3 4)))
  (check-equal? (first-range '((3 4) (2 3))) '((2 3) (3 4)))
  (check-equal? (first-range '((1 4) (1 5))) '((1 5) (1 4)))

  (check-equal? (parse-range "2-3") '(2 3))
  (check-equal? (parse-range "123-512") '(123 512))

  (check-equal? (is-within-range '((2 8) (3 7))) #t)
  (check-equal? (is-within-range '((4 6) (6 6))) #t)
  (check-equal? (is-within-range '((2 4) (6 8))) #f)

  (check-equal? (ranges-intersect '((2 4) (6 8))) #f)
  (check-equal? (ranges-intersect '((2 3) (4 5))) #f)
  (check-equal? (ranges-intersect '((5 7) (7 9))) #t)
  (check-equal? (ranges-intersect '((2 8) (3 7))) #t)
  (check-equal? (ranges-intersect '((4 6) (6 6))) #t)
)
