#lang racket

(define (split-packet input)
  (filter (lambda (v) 
	    (not (equal? v ",")))
	  (regexp-match* #rx"([0-9]+|.)" input)))

(define (parse-packet input)
  (define tokens (split-packet input))
  (let-values ([(_ packet) (line tokens)])
    packet))

(define (parse-packets lines [acc (list)])
  (cond
    [(empty? lines) (reverse acc)]
    [(equal? (first lines) "") (parse-packets (rest lines) acc)]
    [else
      (parse-packets (rest lines) (cons (car (parse-packet (first lines))) acc))]))

(define (line tokens [current (list)])
  (if (empty? tokens) (values '() current)
    (match (first tokens)
      ["[" (let-values ([(remaining subl) (line (rest tokens) (list))])
	     (line remaining (cons subl current)))]
      ["]" (values (rest tokens) (reverse current))]
      [v (line (rest tokens) (cons (string->number v) current))])))

(define (compare ls1 ls2)
  (cond
    [(and (empty? ls1) (empty? ls2))
     #f]
    [(empty? ls1)
     'correct]
    [(empty? ls2)
     'incorrect]
    [else
      (let ([i1 (first ls1)]
	    [i2 (first ls2)])
	(cond
	  [(and (number? i1) (number? i2))
	   (cond
	     [(< i1 i2) 'correct]
	     [(> i1 i2) 'incorrect]
	     [else
	       (compare (rest ls1) (rest ls2))])]
	  [else
	     (or (compare (list-if-num i1) (list-if-num i2))
		 (compare (rest ls1) (rest ls2)))]))]))

(define (list-if-num i)
  (if (list? i) i (list i)))

(define (part-a lines)
  (define packets (parse-packets lines))
  (define (loop ps ind)
    (if (empty? ps) 0
      (+ (if (equal? (compare (first ps) (second ps)) 'correct) ind 0)
	 (loop (drop ps 2) (add1 ind)))))
  (loop packets 1))

(define (get-input)
  (file->lines "day_thirteen.txt"))

(part-a (get-input))

(define (sort-packets packets)
  (sort packets
	(lambda (l1 l2) (equal? (compare l1 l2) 'correct))))

(define (add-divider-packets input)
  (cons "[[2]]"
	(cons "[[6]]"
	      input)))

(define (part-b input)
  (define packets (parse-packets (add-divider-packets input)))
  (define sorted (sort-packets packets))
  (* (add1 (index-of sorted '((6))))
     (add1 (index-of sorted '((2))))))

(part-b (get-input))

(module+ test
  (require rackunit)

  (check-equal? (split-packet "[11,2]") '("[" "11" "2" "]"))
  (check-equal? (split-packet "[11,[2]]") '("[" "11" "[" "2" "]" "]"))

  (check-equal? (parse-packet "[1,2]") '((1 2)))
  (check-equal? (parse-packet "[1,[2,3]]") '((1 (2 3))))

  (define example
    (list
      "[1,1,3,1,1]"
      "[1,1,5,1,1]"
      ""
      "[[1],[2,3,4]]"
      "[[1],4]"
      ""
      "[9]"
      "[[8,7,6]]"
      ""
      "[[4,4],4,4]"
      "[[4,4],4,4,4]"
      ""
      "[7,7,7,7]"
      "[7,7,7]"
      ""
      "[]"
      "[3]"
      ""
      "[[[]]]"
      "[[]]"
      ""
      "[1,[2,[3,[4,[5,6,7]]]],8,9]"
      "[1,[2,[3,[4,[5,6,0]]]],8,9]"
      ))

  (define example-result
    '((1 1 3 1 1)
      (1 1 5 1 1)
      ((1) (2 3 4))
      ((1) 4)
      (9)
      ((8 7 6))
      ((4 4) 4 4)
      ((4 4) 4 4 4)
      (7 7 7 7)
      (7 7 7)
      ()
      (3)
      ((()))
      (())
      (1 (2 (3 (4 (5 6 7)))) 8 9)
      (1 (2 (3 (4 (5 6 0)))) 8 9)))

  (define (loop input expected)
    (cond 
      [(empty? input) '()]
      [(equal? (first input) "") (loop (rest input) expected)]
      [else
	(define result (parse-packet (first input)))
	(check-equal? result (list (first expected)))
	(loop (rest input) (rest expected))]))
  (loop example example-result)

  (define (loop-2 input expected)
    (cond 
      [(empty? input) '()]
      [else
	(check-equal? (compare (first input) (second input)) (first expected))
	(loop-2 (drop input 2) (rest expected))]))
  (loop-2 example-result '(correct correct incorrect correct incorrect correct incorrect incorrect))
  
  (check-equal? (parse-packets example)
		example-result)

  (check-equal? (part-a example) 13)

  (define example-sorted-result
    '(
      ()
      (())
      ((()))
      (1 1 3 1 1)
      (1 1 5 1 1)
      ((1) (2 3 4))
      (1 (2 (3 (4 (5 6 0)))) 8 9)
      (1 (2 (3 (4 (5 6 7)))) 8 9)
      ((1) 4)
      ((2))
      (3)
      ((4 4) 4 4)
      ((4 4) 4 4 4)
      ((6))
      (7 7 7)
      (7 7 7 7)
      ((8 7 6))
      (9)
      ))

  (for-each
    (lambda (left right)
      (check-equal? left right))
    (sort-packets (parse-packets (add-divider-packets example)))
    example-sorted-result)

  (check-equal? (part-b example) 140)
)
