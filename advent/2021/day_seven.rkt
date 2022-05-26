#lang racket

(define (get-comma-numbers line)
  (map string->number (string-split line ",")))

(define (moves-required positions target)
  (map (lambda (p) (abs (- target p))) positions))

(define (total-moves-f positions)
  (lambda (target) (foldl + 0 (moves-required positions target))))

(define (reduce-list xs f)
  (define (do-f x1 x2)
    (if (f x1 x2) x1 x2))
  (foldl do-f (first xs) (rest xs)))

(define (find-smallest-moves positions fuel-cost-f)
  (define furthest (reduce-list positions >))
  (define targets (range 0 (+ furthest 1)))
  (reduce-list 
   (map (lambda (t)
          (foldl + 0
                 (map fuel-cost-f (moves-required positions t)))) targets)
   <))

(define (stepped-fuel-cost moves)
  (foldl + 0 (range 1 (+ moves 1))))

(define (part-a)
  (define positions (get-comma-numbers (first (file->lines "data/DaySeven.txt"))))
  (find-smallest-moves positions (lambda (v) v)))

(define (part-b)
  (define positions (get-comma-numbers (first (file->lines "data/DaySeven.txt"))))
  (find-smallest-moves positions stepped-fuel-cost))

(module+ test
  (require rackunit)

  (check-equal?
   (moves-required (list 1 2 3) 1)
   (list 0 1 2))
  (check-equal?
   (moves-required (list 1 2 3) 2)
   (list 1 0 1))
  (check-equal?
   (moves-required (list 1 2 3) 3)
   (list 2 1 0))

  (define example-positions
    (get-comma-numbers "16,1,2,0,4,2,7,1,2,14"))

  (check-equal?
   (moves-required example-positions 2)
   (list 14 1 0 2 2 0 5 1 0 12))

  (check-equal?
   (reduce-list (list 1 2 3 5 2) >)
   5)
  (check-equal?
   (reduce-list (list 1 2 3 5 2) <)
   1)

  (check-equal?
   (find-smallest-moves example-positions (lambda (v) v))
   37)

  (check-equal?
   (map stepped-fuel-cost (moves-required example-positions 5))
   (list 66 10 6 15 1 6 3 10 6 45))

  (check-equal?
   (find-smallest-moves example-positions stepped-fuel-cost)
   168)
)
