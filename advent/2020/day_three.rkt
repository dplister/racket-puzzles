#lang racket

(provide (all-defined-out))

(require "../grid.rkt")

(define (parse-slope input)
  (parse-coords input (lambda (c) c)))

(define (slide slope mx my)
  (define (loop x y acc)
    (let ([nx (+ x mx)]
          [ny (+ y my)])
      (if (>= ny (grid-height slope))
          (reverse acc)
          (loop nx ny (cons (get-coord slope nx ny #:wrap-x #t)
                            acc)))))
  (loop 0 0 (list (get-coord slope 0 0))))

(define (slide-trees slope mx my)
  (define moves (slide slope mx my))
  (length (filter (lambda (l) (equal? (location-z l) #\#)) moves)))

(define (slide-tree-sets slope trajectories)
  (map (lambda (t) (slide-trees slope (point-x t) (point-y t))) trajectories))

(define (part-a)
  (define slope (parse-slope (file->lines "data/DayThree.txt")))
  (slide-trees slope 3 1))

(define (part-b)
  (define slope (parse-slope (file->lines "data/DayThree.txt")))
  (foldl * 1
         (slide-tree-sets slope
                          (list (point 1 1)
                                (point 3 1)
                                (point 5 1)
                                (point 7 1)
                                (point 1 2)))))

(module+ test
  (require rackunit)

  (check-equal?
   (slide (parse-coords (list "12" "34") (lambda (c) c)) 1 1)
   (list (location 0 0 #\1) (location 1 1 #\4)))

  (define example-data
    (list
     "..##......."
     "#...#...#.."
     ".#....#..#."
     "..#.#...#.#"
     ".#...##..#."
     "..#.##....."
     ".#.#.#....#"
     ".#........#"
     "#.##...#..."
     "#...##....#"
     ".#..#...#.#"))

  (check-equal?
   (slide-trees (parse-slope example-data) 3 1)
   7)

  (check-equal?
   (slide-tree-sets (parse-slope example-data)
                    (list (point 1 1)
                          (point 3 1)
                          (point 5 1)
                          (point 7 1)
                          (point 1 2)))
   (list 2 7 3 4 2))

)
