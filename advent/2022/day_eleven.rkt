#lang racket

(struct monkey (num items operation pass) #:transparent)

(define (add-item m item)
  "gives the monkey an item"
  (monkey (monkey-num m)
	  (append (monkey-items m) (list item))
	  (monkey-operation m)
	  (monkey-pass m)))

(define (add-item-action item)
  "creates a function that can add a specific item to a monkey"
  (lambda (m) (add-item m item)))

(define (remove-item m)
  "removes the first item from the monkey"
  (monkey (monkey-num m)
	  (rest (monkey-items m))
	  (monkey-operation m)
	  (monkey-pass m)))

(define (update-monkey monkies pos action)
  "updates the num monkey with the corresponding action"
  (list-set monkies pos (action (list-ref monkies pos))))

(define (monkey-madness monkies index inspections manage-worry)
  "performs full round of monkey passing"
  (cond
    [(>= index (length monkies)) (values monkies inspections)]
    [else
      (define cm (list-ref monkies index))
      (monkey-madness
	(foldl (lambda (item result)
	       (let ([rev-item (manage-worry (monkey-operation cm) item)])
		 (update-monkey 
		   (update-monkey result ((monkey-pass cm) rev-item) (add-item-action rev-item))
		   index
		   remove-item)))
	     monkies
	     (monkey-items cm))
	(add1 index)
	(hash-update inspections index (lambda (v) (+ v (length (monkey-items cm)))) 0)
	manage-worry)]))

(define (monkey-round monkies inspections rounds [manage-worry worry])
  (if (= rounds 0)
    (values monkies inspections)
    (let-values ([(rev-monkies rev-inspections) (monkey-madness monkies 0 inspections manage-worry)])
      (monkey-round rev-monkies rev-inspections (sub1 rounds)))))

(define (worry op item)
  "calculates the new worry value"
  (quotient (op item) 3))

(define (freaking-out op item)
  "calm blue oceans.. calm blue oceans.."
  (op item))

(define (part-a input)
  (let-values ([(monkies inspections) (monkey-round (parse-monkies input) #hash() 20)])
    (apply * (take
	 (sort (hash-values inspections) >)
	 2))))

(define (get-input)
  (file->lines "day_eleven.txt"))

(define (part-b input)
  (let-values ([(monkies inspections) (monkey-round (parse-monkies input) #hash() 10000 freaking-out)])
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
  (string->number 
    (second (regexp-match #rx"Test: divisible by ([0-9]+)" input))))

(define (parse-pass-to input)
  (string->number (second (regexp-match #rx"throw to monkey ([0-9]+)" input))))

;; execution

(part-a (get-input))

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

  (check-equal? (last (monkey-items (list-ref
				(update-monkey (parse-monkies example) 2 (add-item-action 999))
				2)))
		999)

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
  (check-equal? (part-b example) 2713310158)
)
