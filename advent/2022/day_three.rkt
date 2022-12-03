#lang racket

(define (compartments input)
  "splits input into two character lists"
  (split-at input (/ (length input) 2)))

(define (common-items rucksack)
  "finds the items that are common to both parts of rucksack"
  (let-values ([(left right) (compartments rucksack)])
    (set-intersect left right)))

(define lower-range (list (char->integer #\a) (char->integer #\z)))
(define upper-range (list (char->integer #\A) (char->integer #\Z)))

(define (score item)
  "determines the numerical value of the item"
  (define ival (char->integer item))
  (cond 
    [(and (>= ival (first lower-range))
	  (<= ival (second lower-range)))
     (add1 (- ival (first lower-range)))]
    [else
      (+ (- ival (first upper-range)) 27)]))

(define (score-rucksack rs)
  "scores the duplicate item in the rucksack"
  (apply + (map score (common-items rs))))

(define (total-rucksacks inputs)
  "scores all the rucksack inputs"
  (apply + (map score-rucksack inputs)))

(define (get-input)
  (map string->list (file->lines "day_three.txt")))

(define (part-a)
  (total-rucksacks (get-input)))

(part-a)

; part b

(define (common-group-item rucksacks)
  (apply set-intersect rucksacks))

(define (score-groups lines [acc 0])
  (if (empty? lines) acc
    (score-groups (drop lines 3) 
		  (apply + acc (map score (common-group-item (take lines 3)))))))

(define (part-b)
  (score-groups (get-input)))

(part-b)

(module+ test
  (require rackunit)

  (let-values ([(left right) (compartments (string->list "asDF"))])
    (check-equal? left '(#\a #\s))
    (check-equal? right '(#\D #\F)))

  (define examples
    (map string->list 
	 '("vJrwpWtwJgWrhcsFMMfFFhFp"
	   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
	   "PmmdzqPrVvPwwTWBwg"
	   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
	   "ttgJtRGJQctTZtZT"
	   "CrZsJsPPZsGzwwsLwLmpwMDw")))

  (for-each (lambda (input expected)
	      (check-equal? (common-items input) expected))
	    examples
	    '((#\p)
	      (#\L)
	      (#\P)
	      (#\v)
	      (#\t)
	      (#\s)))

  (for-each (lambda (input expected)
	      (check-equal? (score input) expected))
	    '(#\p #\L #\P #\v #\t #\s)
	    '(16 38 42 22 20 19))

  (check-equal? (total-rucksacks examples) 157)

  (check-equal? (score-groups examples) 70)

)
