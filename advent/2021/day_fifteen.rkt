#lang racket

(require "grid.rkt")
(require "prelude.rkt")

(define (nav g [pts (list (first-point g))] [shortest -1])
  (define adjacents (orthogonal g (first pts)))
  (define valids
    (no-up-if-far-right g (first pts)
                        ; can't cross the same river twice
                        (remove-list adjacents pts)))
  (cond
    ; no viable candidates
    [(empty? valids)
     -1]
    ; if terminus is in reach, terminate branch
    [(find-element valids (last-point g))
     (total-distance (cons (last-point g) pts))]
    [else
     (foldl (lambda (l r)
              (let ([next-pts (cons l pts)])
                (if (and (> r -1)
                         (>= (total-distance next-pts) r))
                    r ; skip if path is already scored higher
                    (let ([next-score (nav g next-pts r)])
                      (if (and (> next-score -1)
                               (or (= r -1)
                                   (< next-score r)))
                          next-score
                          r)))))
            shortest
            valids)]))

(define min-at-least-one
  (lambda xs
    (let ([rs (filter (lambda (v) (> v -1)) xs)])
      (if (empty? rs) -1 (apply min rs)))))

(define (no-up-if-far-right g current-p orths)
  "if the last point is the furthest right, potential orthogaonals can't go up"
  (if (= (point-x current-p)
         (- (grid-width g) 1))
    (filter (lambda (p)
              (not (< (point-y p)
                      (point-y current-p))))
            orths)
    orths))

(define (total-distance pts)
  (foldl
   (lambda (p r) (+ r
                    (if (and (= (point-x p) 0)
                             (= (point-y p) 0))
                        0
                        (point-z p))))
   0
   pts))

(define (find-element pts pt)
  (findf (lambda (p) (equal? p pt)) pts))

(define (part-a)
  (define g (parse-coords (file->lines "data/DayFifteen.txt")))
  (nav g))

(module+ test
  (require rackunit)

  ; check origin is filtered out
  (check-equal?
   (total-distance (list (point 0 0 2)
                         (point 1 0 3)
                         (point 0 1 4)
                         (point 1 1 5)))
   12)

  ; 1 2
  ; 3 4
  (check-equal?
   (nav (parse-coords (list "12" "34"))
        (list (point 1 0 2) (point 0 0 1)))
   6)

  ; 1 5 3
  ; 4 2 6
  ; 7 2 9
  (check-equal?
   (last-point (parse-coords (list "153" "426" "729")))
   (point 2 2 9))
   
  (check-equal?
   (nav (parse-coords (list "153" "426" "729")))
   17)

   (check-equal?
    (nav (parse-coords
          (list
           "1163751742"
           "1381373672"
           "2136511328"
           "3694931569"
           "7463417111"
           "1319128137"
           "1359912421"
           "3125421639"
           "1293138521"
           "2311944581")))
    40)

  (check-equal?
   (min-at-least-one -1 1 2)
   1)

  (check-equal?
   (min-at-least-one -1 -1)
   -1)

  (check-equal?
   (min-at-least-one)
   -1)

  (check-equal?
   (no-up-if-far-right (parse-coords (list "123" "456" "789"))
                       (point 2 1 6)
                       (list (point 2 0 3) (point 1 1 5) (point 2 2 9)))
   (list (point 1 1 5) (point 2 2 9)))

  (check-equal?
   (no-up-if-far-right (parse-coords (list "123" "456" "789"))
                       (point 1 1 5)
                       (list (point 0 1 4) (point 2 1 6)
                             (point 1 0 2) (point 1 2 8)))
   (list (point 0 1 4) (point 2 1 6)
         (point 1 0 2) (point 1 2 8)))

)
