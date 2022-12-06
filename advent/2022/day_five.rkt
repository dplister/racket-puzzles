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

(define (remove-item boxes position [amount 1])
  "Removes a box from column specified"
  (map (lambda (s) (if (= (last s) position) (drop s amount) s)) boxes))

(define (add-item boxes position item)
  "Adds a box to the column specified"
  (map (lambda (s) 
	 (if (= (last s) position) 
	   (if (list? item) (append item s) (cons item s)) 
	   s)) 
       boxes))

(define (get-item boxes position [amount 1])
  "Gets the box for the column specified"
  (take (list-ref boxes (sub1 position)) amount))

(define (get-top-item boxes)
  (flatten (map (lambda (l) (if (> (length l) 1) (first l) '())) boxes)))

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

(define (parse-move input)
  "converts a string into a movement"
  (map string->number (cdr (regexp-match #rx"move ([0-9]+) from ([0-9]+) to ([0-9]+)" input))))

(define (execute-moves stack moves)
  "adjusts stack based on set of move instructions"
  (if (empty? moves) stack
    (execute-moves (execute-move stack (first moves)) (rest moves))))

(define (execute-move stack move)
  "adjusts stack based on move instruction"
  (print-stack stack)
  (displayln move)
  (define amount (first move))
  (add-item (remove-item stack (second move) amount) 
	    (third move) 
	    (get-item stack (second move) amount)))

(define (print-stack stack)
  "draws the stack in a human readable format"
  (define highest (apply max (map length stack)))
  (define top (lambda (p s) 
		(if (>= p (length s)) "   " (format "[~a]" (list-ref (reverse s) p)))))
  (for-each (lambda (row) 
	      (for-each (lambda (column)
			  (display (format "~a " (top row column))))
			stack)
	      (displayln ""))
	    (range highest -1 -1)))

(define (split-input lines)
  "converts input into stack and moves"
  (splitf-at lines (lambda (l)
		     (non-empty-string? (string-trim l)))))

(define (get-input)
  (file->lines "day_five.txt"))

(define (part-a input)
  (let-values ([(stack moves) (split-input input)])
    (get-top-item (execute-moves 
		    (parse-stacks stack) 
		    (unwind-moves (map parse-move (rest moves)))))))

; (part-a (get-input))

(define (unwind-moves moves)
  "ensures each move only moves 1 box"
  (cond
    [(empty? moves) '()]
    [(= (first (first moves)) 1)
     (cons (first moves) 
	   (unwind-moves (rest moves)))]
    [else
      (define fs (first moves))
      (cons (cons 1 (rest fs))
	    (unwind-moves (cons (list-update fs 0 sub1)
				(rest moves))))]))

(define (part-b input)
  (let-values ([(stack moves) (split-input input)])
    (get-top-item (execute-moves 
		    (parse-stacks stack) 
		    (map parse-move (rest moves))))))

(part-b (get-input))

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

  (check-equal? (parse-move "move 1 from 2 to 1") '(1 2 1))
  (check-equal? (parse-move "move 3 from 1 to 3") '(3 1 3))
  (check-equal? (parse-move "move 2 from 2 to 1") '(2 2 1))
  (check-equal? (parse-move "move 1 from 1 to 2") '(1 1 2))

  (check-equal? (unwind-moves '((2 1 1))) '((1 1 1) (1 1 1)))

  (define example-input '("    [D]    "
			  "[N] [C]    "
			  "[Z] [M] [P]"
			  " 1   2   3 "
			  ""
			  "move 1 from 2 to 1"
			  "move 3 from 1 to 3"
			  "move 2 from 2 to 1"
			  "move 1 from 1 to 2"))

  (check-equal? (part-a example-input)
		`("C" "M" "Z"))

  (check-equal? (part-b example-input)
		'("M" "C" "D"))
)

