#lang racket

(define (parse-stacks rows)
  "takes rows of input and generates stacks"
  (if (= (length rows) 1)
    (map list (map string->number (string-split (first rows))))
    (zip
      (parse-boxes (first rows))
      (parse-stacks (cdr rows)))))

(define (zip boxes bases)
  "Adds boxes to bases if not empty"
  (map (lambda (bx bs) (if (not (empty? bx)) (append bx bs) bs))
       boxes bases))

(define (remove-item boxes position)
  "Removes a box from column specified"
  (map (lambda (s) (if (= (last s) position) (rest s) s)) boxes))

(define (add-item boxes position item)
  "Adds a box to the column specified"
  (map (lambda (s) (if (= (last s) position) (cons item s) s)) boxes))

(define (get-item boxes position)
  "Gets the box for the column specified"
  (first (list-ref boxes (add1 position))))

(define (get-top-item boxes)
  (flatten (map (lambda (l) (if (> (length l) 1) (first boxes) '())) boxes)))

(define (parse-boxes line)
  "converts a line of input into a set of boxes"
  (define (parse-tokens tokens [acc (list)])
    (if (empty? tokens) (reverse acc)
      (parse-tokens (drop tokens (min (length tokens) 4))
		    (cons (parse-box (list->string (take tokens 3))) acc))))
  (parse-tokens (string->list line)))

(define (parse-box input)
  "converts a string into a box input"
  (if (non-empty-string? (string-trim input))
    (list (substring input 1 (sub1 (string-length input))))
    (list)))

(define (parse-movement input)
  "converts a string into a movement"
  (map string->number (cdr (regexp-match #rx"move ([0-9]+) from ([0-9]+) to ([0-9]+)" input))))

(define (execute-moves stack moves)
  "adjusts stack based on set of move instructions"
  (if (empty? moves) stack
    (execute-moves (execute-move stack (parse-movement (first moves))) (rest moves))))

(define (execute-move stack move)
  "adjusts stack based on move instruction"
  (if (= (first move) 0) stack
      (define item (get-item stack (second move)))
      (execute-move
	(add-item (remove-item stack (second move)) (third move) item)
	(list-update stack 0 sub1))))

(define (split-input lines)
  "converts input into stack and moves"
  (splitf-at lines (lambda (l)
		     (non-empty-string? (string-trim l)))))

(define (get-input)
  (file->lines "day_five.txt"))

(define (part-a input)
  (let-values ([(stack moves) (split-input input)])
    (get-top-item (execute-moves stack (rest moves)))))

(module+ test
  (require rackunit)

  (check-equal? (parse-box "[X]") '("X"))
  (check-equal? (parse-box "[ABC]") '("ABC"))
  (check-equal? (parse-box "   ") '())

  (check-equal? (parse-boxes "[Z] [M] [P]") '(("Z") ("M") ("P")))
  (check-equal? (parse-boxes "[N] [C]    ") '(("N") ("C") ()))
  (check-equal? (parse-boxes "    [D]    ") '(() ("D") ()))

  (check-equal? (zip '(("A") () ("C")) '((1) (2) (3))) '(("A" 1) (2) ("C" 3)))

  (check-equal? (parse-stacks 
		  (list "    [D]    "
			"[N] [C]    "
			"[Z] [M] [P]"
			" 1   2   3 "))
		'(("N" "Z" 1) ("D" "C" "M" 2) ("P" 3)))

  (check-equal? (parse-movement "move 1 from 2 to 1") '(1 2 1))
  (check-equal? (parse-movement "move 3 from 1 to 3") '(3 1 3))
  (check-equal? (parse-movement "move 2 from 2 to 1") '(2 2 1))
  (check-equal? (parse-movement "move 1 from 1 to 2") '(1 1 2))

  (check-equal? (part-a '("    [D]    "
			  "[N] [C]    "
			  "[Z] [M] [P]"
			  " 1   2   3 "
			  ""
			  "move 1 from 2 to 1"
			  "move 3 from 1 to 3"
			  "move 2 from 2 to 1"
			  "move 1 from 1 to 2"))
		`("C" "M" "Z"))
)

