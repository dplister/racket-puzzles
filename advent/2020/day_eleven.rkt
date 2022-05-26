#lang racket

(require "../prelude.rkt")
(require "../grid.rkt")

(define (step seating adjust-seat)
  "generates a map of the updated seating arrangements"
  (define (loop ds x y)
    (cond
      [(= y (grid-height seating)) ds]
      [else
       (loop (set-grid-coord ds x y (adjust-seat seating x y))
             (if (>= (add1 x) (grid-width seating))
                 0 (add1 x))
             (if (>= (add1 x) (grid-width seating))
                 (add1 y) y))]))
  (loop (empty-grid) 0 0))

(define (step-f adjust-seat)
  "currys step with adjust-seat"
  (lambda (seating) (step seating adjust-seat)))

(define (repeat-steps seating adjust-seat)
  (repeat-list-until-trans seating (step-f adjust-seat)
                           (lambda (fs sc) (not (equal? fs sc)))))

(define (surrounding-seats seating x y target)
  "lists the seats around x y that match target"
  (filter (lambda (l) (equal? (location-z l) target))
          (surrounding seating (point x y))))

(define (sighted-seats seating x y)
  "lists the seats found in each sighted direction"
  (filter-map
   (lambda (p)
     (last
      (line seating (+ x (first p)) (+ y (second p)) (first p) (second p)
            (lambda (l) (not (or (not l)
                                 (equal? (location-z l) #\#)
                                 (equal? (location-z l) #\L)))) ; unoccupied chairs block view
            #:include-break #t)))
   (append cardinal-points orthogonal-points)))

(define (near-seat seating x y)
  "determines if there is a seat, and if it should be occupied, based on adjacent seats"
  (match (location-z (get-coord seating x y))
    [#\. #\.]
    [#\L (if (empty? (surrounding-seats seating x y #\#))
             #\# #\L)]
    [#\# (if (>= (length (surrounding-seats seating x y #\#)) 4)
             #\L #\#)]))

(define (sighted-seat seating x y)
  "determines if there is a seat, and if it should be occupied, based on sight-level seats"
  (match (location-z (get-coord seating x y))
    [#\. #\.]
    [#\L (if (empty? (occupied-seats-filter (sighted-seats seating x y)))
             #\# #\L)]
    [#\# (if (>= (length (occupied-seats-filter (sighted-seats seating x y))) 5)
             #\L #\#)]))

(define (occupied-seats-filter ls)
  "filters locations to only those that have occupied seats"
  (filter (lambda (loc) (equal? (location-z loc) #\#)) ls))

(define (count-occupied seating)
  (length (filter (lambda (v) (equal? v #\#)) (all-seats seating))))

(define (all-seats seating)
  (map location-z (all-points seating)))

(define (part-a)
  (define initial (parse-coords (file->lines "data/DayEleven.txt") identity))
  (count-occupied (repeat-steps initial near-seat)))

(define (part-b)
  (define initial (parse-coords (file->lines "data/DayEleven.txt") identity))
  (count-occupied (repeat-steps initial sighted-seat)))

(module+ test
  (require rackunit)

  (define initial-example
    (list "L.LL.LL.LL"
          "LLLLLLL.LL"
          "L.L.L..L.."
          "LLLL.LL.LL"
          "L.LL.LL.LL"
          "L.LLLLL.LL"
          "..L.L....."
          "LLLLLLLLLL"
          "L.LLLLLL.L"
          "L.LLLLL.LL"))

  (define example-steps
    (list (list "#.##.##.##"
                "#######.##"
                "#.#.#..#.."
                "####.##.##"
                "#.##.##.##"
                "#.#####.##"
                "..#.#....."
                "##########"
                "#.######.#"
                "#.#####.##")
          (list "#.LL.L#.##"
                "#LLLLLL.L#"
                "L.L.L..L.."
                "#LLL.LL.L#"
                "#.LL.LL.LL"
                "#.LLLL#.##"
                "..L.L....."
                "#LLLLLLLL#"
                "#.LLLLLL.L"
                "#.#LLLL.##")
          (list "#.##.L#.##"
                "#L###LL.L#"
                "L.#.#..#.."
                "#L##.##.L#"
                "#.##.LL.LL"
                "#.###L#.##"
                "..#.#....."
                "#L######L#"
                "#.LL###L.L"
                "#.#L###.##")
          (list "#.#L.L#.##"
                "#LLL#LL.L#"
                "L.L.L..#.."
                "#LLL.##.L#"
                "#.LL.LL.LL"
                "#.LL#L#.##"
                "..L.L....."
                "#L#LLLL#L#"
                "#.LLLLLL.L"
                "#.#L#L#.##")
          (list "#.#L.L#.##"
                "#LLL#LL.L#"
                "L.#.L..#.."
                "#L##.##.L#"
                "#.#L.LL.LL"
                "#.#L#L#.##"
                "..L.L....."
                "#L#L##L#L#"
                "#.LLLLLL.L"
                "#.#L#L#.##")))

  (check-equal?
   (step (parse-coords initial-example identity) near-seat)
   (parse-coords (first example-steps) identity))

  (check-equal?
   (step (step (parse-coords initial-example identity) near-seat) near-seat)
   (parse-coords (second example-steps) identity))

  (for ([n (range 1 5)])
    (check-equal?
     (repeat-list (parse-coords initial-example identity) (step-f near-seat) n)
     (parse-coords (first (drop example-steps (sub1 n))) identity)))

  (check-equal?
   (count-occupied (parse-coords (last example-steps) identity))
   37)

  (check-equal?
   (repeat-steps (parse-coords initial-example identity) near-seat)
   (parse-coords (last example-steps) identity))

  (check-equal?
   (length (sighted-seats
    (parse-coords (list ".......#."
                        "...#....."
                        ".#......."
                        "........."
                        "..#L....#"
                        "....#...."
                        "........."
                        "#........"
                        "...#.....")
                  identity)
    3 4))
   8)

  (check-equal?
   (location-z
    (first (sighted-seats
            (parse-coords (list "............."
                                ".L.L.#.#.#.#."
                                ".............")
                          identity)
            1 1)))
   #\L)

  (define example-steps-sighting
    (list
     (list "#.##.##.##"
           "#######.##"
           "#.#.#..#.."
           "####.##.##"
           "#.##.##.##"
           "#.#####.##"
           "..#.#....."
           "##########"
           "#.######.#"
           "#.#####.##")
     (list "#.LL.LL.L#"
           "#LLLLLL.LL"
           "L.L.L..L.."
           "LLLL.LL.LL"
           "L.LL.LL.LL"
           "L.LLLLL.LL"
           "..L.L....."
           "LLLLLLLLL#"
           "#.LLLLLL.L"
           "#.LLLLL.L#")
     (list "#.L#.##.L#"
           "#L#####.LL"
           "L.#.#..#.."
           "##L#.##.##"
           "#.##.#L.##"
           "#.#####.#L"
           "..#.#....."
           "LLL####LL#"
           "#.L#####.L"
           "#.L####.L#")
     (list "#.L#.L#.L#"
           "#LLLLLL.LL"
           "L.L.L..#.."
           "##LL.LL.L#"
           "L.LL.LL.L#"
           "#.LLLLL.LL"
           "..L.L....."
           "LLLLLLLLL#"
           "#.LLLLL#.L"
           "#.L#LL#.L#")
     (list "#.L#.L#.L#"
           "#LLLLLL.LL"
           "L.L.L..#.."
           "##L#.#L.L#"
           "L.L#.#L.L#"
           "#.L####.LL"
           "..#.#....."
           "LLL###LLL#"
           "#.LLLLL#.L"
           "#.L#LL#.L#")
     (list "#.L#.L#.L#"
           "#LLLLLL.LL"
           "L.L.L..#.."
           "##L#.#L.L#"
           "L.L#.LL.L#"
           "#.LLLL#.LL"
           "..#.L....."
           "LLL###LLL#"
           "#.LLLLL#.L"
           "#.L#LL#.L#")))
   
  (check-equal?
   (step (parse-coords initial-example identity) sighted-seat)
   (parse-coords (first example-steps-sighting) identity))

  (check-equal?
   (step (step (parse-coords initial-example identity) sighted-seat) sighted-seat)
   (parse-coords (second example-steps-sighting) identity))

  (for ([n (range 1 6)])
    (check-equal?
     (repeat-list (parse-coords initial-example identity) (step-f sighted-seat) n)
     (parse-coords (first (drop example-steps-sighting (sub1 n))) identity)))

  (check-equal?
   (repeat-steps (parse-coords initial-example identity) sighted-seat)
   (parse-coords (last example-steps-sighting) identity))
)
