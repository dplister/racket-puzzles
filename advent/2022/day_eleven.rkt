#lang racket

(struct monkey (num [items #:mutable] operation pass) #:transparent)

(define (add-item m item)
  "gives the monkey an item"
  (set-monkey-items! m (cons item (monkey-items m))))

(define (remove-item m)
  "removes the first item from the monkey"
  (set-monkey-items! m (rest (monkey-items m))))

(define (monkey-madness monkies index inspections manage-worry)
  "performs full round of monkey passing"
  (cond
    [(>= index (length monkies)) (values monkies inspections)]
    [else
      (define cm (list-ref monkies index))
      (hash-update! inspections index (lambda (v) (+ v (length (monkey-items cm)))) 0)
      (map (lambda (item)
	     (let* ([rev-item (manage-worry (monkey-operation cm) item)]
		    [pass-to (list-ref monkies ((monkey-pass cm) rev-item))])
	       (add-item pass-to rev-item)
	       (remove-item cm)))
	   (monkey-items cm))
      (monkey-madness
	monkies
	(add1 index)
	inspections
	manage-worry)]))

(define (monkey-round monkies inspections rounds [manage-worry worry])
  (if (= rounds 0)
    (values monkies inspections)
    (let-values ([(rev-monkies rev-inspections) (monkey-madness monkies 0 inspections manage-worry)])
      (monkey-round rev-monkies rev-inspections (sub1 rounds)))))

(define (worry op item)
  "calculates the new worry value"
  (quotient (op item) 3))

(define (freaking-out-binding divisor)
  "calm blue oceans.. calm blue oceans.."
  (lambda (op item)
    (modulo (op item) divisor)))

(define (part-a input)
  (let-values ([(monkies inspections) (monkey-round (parse-monkies input) (make-hash) 20)])
    (apply * (take
	 (sort (hash-values inspections) >)
	 2))))

(define (get-input)
  (file->lines "day_eleven.txt"))

(define (part-b input)
  (let-values ([(monkies inspections) 
		(monkey-round (parse-monkies input) 
			      (make-hash) 
			      10000 
			      (freaking-out-binding (get-common-divisor input)))])
    (apply * (take
	 (sort (hash-values inspections) >)
	 2))))

;; parsing

(define (parse-monkies lines)
  (if (empty? lines) '()
    (cons (parse-monkey (take lines 6))
	  (parse-monkies (drop lines (min (length lines) 7))))))

(define (parse-monkey lines)
  (monkey 
    (parse-name (first lines))
    (parse-starting-items (second lines))
    (parse-operation (third lines))
    (parse-pass (drop lines 3))))

(define (parse-name input)
  (string->number 
    (second 
      (regexp-match #rx"Monkey ([0-9]+):" input))))

(define (parse-starting-items input)
  (map string->number 
       (regexp-split ", " 
		     (second (regexp-match #rx"Starting items: (.*)" input)))))

(define (parse-operation input)
  (define line (second (regexp-match #rx"Operation: new = (.*)" input)))
  (define tokens (string-split line))
  (lambda (old)
    ((string->op (second tokens))
     (if (equal? (first tokens) "old") old (string->number (first tokens)))
     (if (equal? (third tokens) "old") old (string->number (third tokens))))))

(define (string->op input)
  (match input
    ["+" +]
    ["/" /]
    ["*" *]
    ["-" -]))

(define (parse-pass lines)
  (lambda (current)
    (if (= (modulo current (parse-divisible (first lines))) 0)
      (parse-pass-to (second lines))
      (parse-pass-to (third lines)))))

(define (parse-divisible input)
  (define m (regexp-match #rx"Test: divisible by ([0-9]+)" input))
  (and m (string->number (second m))))

(define (parse-pass-to input)
  (string->number (second (regexp-match #rx"throw to monkey ([0-9]+)" input))))

(define (get-common-divisor lines)
  "gets the set of divisors"
  (define values (filter-map (lambda (v) (parse-divisible v)) lines))
  (apply * values))

;; execution

(part-a (get-input))

(part-b (get-input))

(module+ test
  (require rackunit)

  (check-equal? (parse-name "Monkey 0:") 0)
  (check-equal? (parse-starting-items "Starting items: 79, 98") '(79 98))
  (check-equal? ((parse-operation "Operation: new = old * 19") 1) 19)
  (define sample-pass
    (list
      "Test: divisible by 23"
      "If true: throw to monkey 2"
      "If false: throw to monkey 3"))
  (check-equal? ((parse-pass sample-pass) 23) 2)
  (check-equal? ((parse-pass sample-pass) 24) 3)

  (define example 
    (list "Monkey 0:"
	  "Starting items: 79, 98"
	  "Operation: new = old * 19"
	  "Test: divisible by 23"
	  "If true: throw to monkey 2"
	  "If false: throw to monkey 3"
	  ""
	  "Monkey 1:"
	  "Starting items: 54, 65, 75, 74"
	  "Operation: new = old + 6"
	  "Test: divisible by 19"
	  "If true: throw to monkey 2"
	  "If false: throw to monkey 0"
	  ""
	  "Monkey 2:"
	  "Starting items: 79, 60, 97"
	  "Operation: new = old * old"
	  "Test: divisible by 13"
	  "If true: throw to monkey 1"
	  "If false: throw to monkey 3"
	  ""
	  "Monkey 3:"
	  "Starting items: 74"
	  "Operation: new = old + 3"
	  "Test: divisible by 17"
	  "If true: throw to monkey 0"
	  "If false: throw to monkey 1"))

  (check-equal? (map monkey-num (parse-monkies example)) '(0 1 2 3))

  (let ([monkey 
	  (list-ref
	    (parse-monkies example) 
	    2)])
    (add-item monkey 999)
    (check-equal? (last (monkey-items monkey)) 
		  999))

  (let-values ([(monkies inspections) (monkey-madness (parse-monkies example) 0 #hash() worry)])
    (for-each (lambda (m v)
		(check-equal? (monkey-items m) v))
	      monkies
	      '((20 23 27 26)
		(2080 25 167 207 401 1046)
		()
		())))

  (let-values ([(monkies inspections) (monkey-round (parse-monkies example) #hash() 20)])
    (check-equal? inspections #hash((0 . 101) (1 . 95) (2 . 7) (3 . 105))))

  (check-equal? (part-a example) 10605)

  (check-equal? (get-common-divisor example) 96577)

  (check-equal? (part-b example) 2713310158)
)
