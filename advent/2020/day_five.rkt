#lang racket

(require "../prelude.rkt")

(define (letter c start end)
  (match c
    [#\F (lower start end)]
    [#\B (upper start end)]
    [#\L (lower start end)]
    [#\R (upper start end)]))

(define (lower start end)
  (if (equal? (distance start end) 1) (list start start)
      (list start (- end (round (/ (- end start) 2))))))

(define (upper start end)
  (if (equal? (distance start end) 1) (list end end)
      (list (+ start (round (/ (- end start) 2))) end)))

(define (locate-seat pass row column)
  (cond
    [(empty? pass) (list (first row) (first column))]
    [(member (first pass) (list #\F #\B))
     (locate-seat (rest pass) (letter (first pass) (first row) (second row)) column)]
    [(member (first pass) (list #\L #\R))
     (locate-seat (rest pass) row (letter (first pass) (first column) (second column)))]))

(define (seat-number row column)
  (+ (* row 8) column))

(define (part-a)
  (define loc (lambda (p) (locate-seat p (list 0 127) (list 0 7))))
  (define vals (map loc (map string->list (file->lines "data/DayFive.txt"))))
  (first (sort (map (lambda (v) (seat-number (first v) (second v))) vals) >)))

(define (part-b)
  (define loc (lambda (p) (locate-seat p (list 0 127) (list 0 7))))
  (define vals (map loc (map string->list (file->lines "data/DayFive.txt"))))
  (find-gap (map (lambda (v) (seat-number (first v) (second v))) vals)))

(define (find-gap lst)
  "locates a missing digit in a list of numbers"
  (define sl (sort lst <))
  (define (loop prev curr)
    (cond
      [(empty? curr) #f]
      [(not (equal? (sub1 (first curr)) prev)) (sub1 (first curr))]
      [else (loop (first curr) (rest curr))]))
  (loop (first sl) (rest sl)))
  
(module+ test
  (require rackunit)

  (for ([inputs '((0 127) (32 63) (4 7) (0 63) (32 47) (0 7))]
        [expected '((0 63) (32 47) (4 5) (32 63) (40 47) (4 7))]
        [op (list lower lower lower upper upper upper)])
    (let ([result (op (first inputs) (second inputs))])
      (check-equal? result expected)))

  (for ([input (map string->list '("FBFBBFFRLR" "BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"))]
        [expected (list (list 44 5) (list 70 7) (list 14 7) (list 102 4))])
    (check-equal?
     (locate-seat input (list 0 127) (list 0 7))
     expected))

  (for ([input '((44 5) (70 7) (14 7) (102 4))]
        [expected '(357 567 119 820)])
    (check-equal?
     (seat-number (first input) (second input))
     expected))

  (check-equal?
   (find-gap (list 1 2 4 5))
   3)
  (check-equal?
   (find-gap (list 5 4 1 2))
   3)
  (check-equal?
   (find-gap (list 1 2 3 5))
   4)
)
