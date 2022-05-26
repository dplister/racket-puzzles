#lang racket

(require "../grid.rkt")
(require "../prelude.rkt")

(provide (all-defined-out))

(define (parse-enhancement line)
  (list->vector (string->list (string-trim line))))

(define (enhance g e inf)
  "expands the grid g and enhances e its set of points"
  (define lg (expand-grid g 3 inf))
  (write-file "map.txt" (format-grid lg))
  (define mx (- (grid-width lg) 2))
  (define my (- (grid-height lg) 2))
  (define uppt (pfill 1 1 mx my))
  (define elg
    (foldl (lambda (p r)
             (let ([x (point-x p)]
                   [y (point-y p)])
               (set-grid-coord r x y 
                               (resolve-point lg e x y))))
           lg
           uppt))
  ; set the outer border without whatever 1,1 was
  (define refz (location-z (get-coord elg 1 1)))
  (foldl (lambda (p r)
           (set-grid-coord r (point-x p) (point-y p) refz))
         elg
         (flatten (border elg 1))))

(define (current-inf g)
  (location-z (get-coord g 0 0)))
  
(define (resolve-point g e x y)
  (vector-ref e (chars-to-binary-value (fetch-box g x y))))

(define (fetch-box g x y)
  "fetches the coordinate's boxmates and returns the set of their values"
  (map location-z (flatten (box-lines g (point x y) 1))))

(define (chars-to-binary-value l)
  (binary->number (map char-to-bin l)))

(define (char-to-bin c)
  (match c
    [#\. 0]
    [#\# 1]))

(define (part-a)
  (define input (file->lines "data/DayTwenty.txt"))
  (define en (parse-enhancement (first input)))
  (define mp (parse-coords (drop input 2) (lambda (v) v)))
  (define step-1 (enhance mp en #\.))
  (define step-2 (enhance step-1 en (current-inf step-1)))
  (write-file "map-enhanced.txt" (format-grid step-2))
  (count-pixels step-2))

(define (part-b)
  (define input (file->lines "data/DayTwenty.txt"))
  (define en (parse-enhancement (first input)))
  (define mp (parse-coords (drop input 2) (lambda (v) v)))
  (count-pixels (enhance-repeat mp en 50 #\.)))

(define (count-pixels g)
  (foldl (lambda (l r) (+ r (if (equal? (location-z l) #\#) 1 0)))
         0 (dict-values (grid-points g))))

(define (enhance-repeat mp en n [inf-val #f])
  (if (= n 0) mp
      (enhance-repeat
       (enhance mp en (or inf-val (current-inf mp)))
       en
       (- n 1))))
             
(module+ test
  (require rackunit)

  (define example-algo
    (parse-enhancement "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"))

  (define example-map
    (parse-coords
     (list "#..#."
           "#...."
           "##..#"
           "..#.."
           "..###")
     (lambda (c) c)))

  (check-equal?
   (resolve-point example-map example-algo 2 2)
   #\#)

  (check-equal?
   (enhance example-map example-algo #\.)
   (parse-coords 
    (list "..........."
          "..........."
          "...##.##..."
          "..#..#.#..."
          "..##.#..#.."
          "..####..#.."
          "...#..##..."
          "....##..#.."
          ".....#.#..."
          "..........."
          "...........")
    (lambda (v) v)))

  (let* ([st1 (enhance example-map example-algo #\.)]
         [st2 (enhance st1 example-algo (current-inf st1))])
    ; (displayln (format-grid st2))
    (check-equal? st2
                  (parse-coords 
                   (list "................."
                         "................."
                         "................."
                         "................."
                         "...........#....."
                         ".....#..#.#......"
                         "....#.#...###...."
                         "....#...##.#....."
                         "....#.....#.#...."
                         ".....#.#####....."
                         "......#.#####...."
                         ".......##.##....."
                         "........###......"
                         "................."
                         "................."
                         "................."
                         ".................")
                   (lambda (v) v))))

  (check-equal?
   (count-pixels (enhance-repeat example-map example-algo 50 #\.))
   3351)
  )
