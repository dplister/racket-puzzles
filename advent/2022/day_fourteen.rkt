#lang racket

(require "grid.rkt")

(define (parse-lines inputs)
  "converts input set into a set of point sets"
  (define (loop ls)
    (if (<= (length ls) 1) (list)
      (append (apply draw-line (append (first ls) (second ls)))
	      (loop (rest ls)))))
  (flatten (map loop (map parse-line inputs))))

(define (parse-line input)
  "converts a line into a set of points"
  (map parse-point (string-split input " -> ")))

(define (parse-point input)
  "converts a point definition into a set of numbers"
  (map string->number (string-split input ",")))

(define (grid-from-points points)
  "creates a grid that fits in the set of points provided"
  (define offset-x (apply min (map point-x points)))
  (define max-x (apply max (map point-x points)))
  (define max-y (apply max (map point-y points)))
  (foldl 
    (lambda (p result) 
      (set-point 
        result 
        (- (point-x p) offset-x)
        (point-y p)
        #\#))
    (init-grid (+ 1 (- max-x offset-x)) 
               (+ 1 max-y) 
               #\.)
    points))

(define (drop-sand g x y)
  "places a drop of sand, returns updated board or #f if placement failed"
  (define next
    (or (and (>= (add1 y) (grid-height g)) 'fell)
        (available-spot? g x (add1 y)) ; down
        (and (< (sub1 x) 0) 'fell)
        (available-spot? g (sub1 x) (add1 y)) ; down-left
        (and (= (add1 x) (grid-width g)) 'fell)
        (available-spot? g (add1 x) (add1 y)))) ; down-right
  ; (draw-grid g)
  (cond
    [(equal? next 'fell) #f]
    [(equal? next #f)
     ; (displayln (format "sand pos: ~v ~v" x y))
     (set-point g x y #\o)]
    [else
      (drop-sand g (first next) (second next))]))

(define (available-spot? g x y)
  "returns (x y) coordinates if empty spot or #f"
  (and (equal? (get-point g x y) #\.) (list x y)))

(define (run inputs)
  (define pts (parse-lines inputs))
  (define g (grid-from-points pts))
  (define sand-pos (- 500 (apply min (map point-x pts))))
  (displayln (format "sand-pos ~v" sand-pos))
  (define (loop gs)
    (let ([s (drop-sand gs sand-pos 0)])
      (if (equal? s #f)
        (count-sand gs)
        (loop gs))))
  (define score (loop g))
  (draw-grid g)
  score)

(define (count-sand gs)
  "finds all points with sand"
  (length 
    (filter 
      (lambda (v)
        (equal? v #\o))
      (vector->list (grid-points gs)))))

(define (get-input)
  (file->lines "day_fourteen.txt"))

(define (part-a)
  (run (get-input)))

; (part-a)

(define (create-catchment pts)
  "creates a long catch under the points provided"
  (define y (+ 2 (apply max (map point-y pts))))
  (map (lambda (x) (point x y))
       (range 0 1000)))

(define (run-2 inputs)
  (define pts (parse-lines inputs))
  (define all-pts (append pts (create-catchment pts)))
  (define g (grid-from-points all-pts))
  (define sand-pos 500)
  (define (loop gs)
    (cond
      [(equal? (get-point gs 500 0) #\o)
       (count-sand gs)]
      [else
        (drop-sand gs sand-pos 0)
        (loop gs)]))
  (define result (loop g))
  (draw-grid g)
  result)

(define (part-b)
  (run-2 (get-input)))

(part-b)

(module+ test
  (require rackunit)

  (check-equal? (parse-point "123,4") '(123 4))
  (check-equal? (parse-point "321,6") '(321 6))

  (define example
    (list "498,4 -> 498,6 -> 496,6"
	  "503,4 -> 502,4 -> 502,9 -> 494,9"))

  (check-equal? (parse-line (first example)) '((498 4) (498 6) (496 6)))
  (check-equal? (parse-line (second example)) '((503 4) (502 4) (502 9) (494 9)))

  ;(draw-grid (grid-from-points (parse-lines example)))
  (check-equal? (run example) 24)
  (check-equal? (run-2 example) 93)
)
