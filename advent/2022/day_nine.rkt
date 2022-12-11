#lang racket

(struct point (x y) #:transparent)

; move the head
(define (move-head pos mov)
  "moves the pos by mov"
  (point (+ (point-x pos) (point-x mov))
	 (+ (point-y pos) (point-y mov))))

(define (move moves heads tails)
  "moves the head and tail exactly one move"
  (cond
    [(empty? moves) (values heads tails)]
    [else
      (define hp (move-head (first heads) (first moves)))
      (move
	(rest moves)
	(cons hp heads)
	(follow-tails hp tails))]))

(define (follow-tails leader tails)
  "updates the set of tail items based on the leader"
  (cond 
    [(empty? tails) '()]
    [else 
      (define nl (follow leader (first (first tails))))
      (cons
	(cons nl (first tails))
	(follow-tails nl (rest tails)))]))

(define (identical-position? p x y)
  "determines if point matches x y coordinates"
  (and (= (point-x p) x)
       (= (point-y p) y)))

(define (draw-position head tail)
  (for-each 
    (lambda (y) 
      (for-each 
	(lambda (x)
	  (display (cond
		     [(identical-position? head x y) "H"]
		     [(identical-position? tail x y) "T"]
		     [else "."])))
	(range -5 5 1))
      (displayln ""))
    (range -5 5 1))
  (displayln ""))

(define (follow head tail)
  "generates the tail's new position based on head"
  (define xd (- (point-x head) (point-x tail)))
  (define yd (- (point-y head) (point-y tail)))
  (define both-different 
    (and  
      ; both different coords
      (not (or (= (point-x head) (point-x tail))
	       (= (point-y head) (point-y tail))))
      ; not just touching on the diag
      (or (> (abs xd) 1)
	  (> (abs yd) 1))))
  (point (+ (point-x tail) (shrink xd #:include-near both-different))
	 (+ (point-y tail) (shrink yd #:include-near both-different))))

(define (shrink num #:include-near near)
  "converts a large number in either direction to within 1 of 0"
  (cond
    [(< num -1) -1]
    [(> num 1) 1]
    [(and near (= num -1)) -1]
    [(and near (= num 1)) 1]
    [else 0]))

(define (parse-moves lines)
  (append-map parse-move lines))

(define (parse-move input)
  "converts a direction number into a set of point movements"
  (define tokens (rest (regexp-match #rx"([RULD]) ([0-9]+)" input)))
  (make-list (string->number (second tokens))
	     (dir->point (first tokens))))

(define (dir->point input)
  "converts a direction into a point"
  (match input
    ["U" (point 0 -1)]
    ["D" (point 0 1)]
    ["L" (point -1 0)]
    ["R" (point 1 0)]))

(define (run lines tail-amount)
  (define moves (parse-moves lines))
  (define-values (head tail) 
    (move moves 
	  (list (point 0 0))
	  (make-list tail-amount (list (point 0 0)))))
  (length (remove-duplicates (last tail))))

(define (get-input)
  (file->lines "day_nine.txt"))

(define (part-a)
  (run (get-input) 1))

(part-a)

(define (part-b)
  (run (get-input) 9))

(part-b)

(module+ test
  (require rackunit)

  ; cardinal movement examples
  (check-equal? (follow (point 3 1) (point 1 1))
		(point 2 1))
  (check-equal? (follow (point 1 3) (point 1 1))
		(point 1 2))

  ; diagonals
  (check-equal? (follow (point 2 1) (point 1 3))
		(point 2 2))
  (check-equal? (follow (point 3 2) (point 1 3))
		(point 2 2))

  ; move parsing
  (check-equal? (parse-move "L 4")
		(list (point -1 0) (point -1 0) (point -1 0) (point -1 0)))
  (check-equal? (parse-moves (list "U 1" "R 2"))
		(list (point 0 -1) (point 1 0) (point 1 0)))

  (define example
    (list "R 4"
	  "U 4"
	  "L 3"
	  "D 1"
	  "R 4"
	  "D 1"
	  "L 5"
	  "R 2"))
  (check-equal? (run example 1) 13)

  (check-equal? (run example 9) 1)

  (define example-2
    (list "R 5"
	  "U 8"
	  "L 8"
	  "D 3"
	  "R 17"
	  "D 10"
	  "L 25"
	  "U 20"))
  (check-equal? (run example-2 9) 36)
)
