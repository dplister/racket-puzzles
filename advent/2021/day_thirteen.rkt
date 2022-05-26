#lang racket

(struct point (x y) #:transparent)

(define (parse-point line)
  (define tokens (string-split line ","))
  (point (string->number (first tokens)) (string->number (second tokens))))

(define (parse-bend line)
  (define tokens (string-split (third (string-split line)) "="))
  (match (first tokens)
    ["x" (point (string->number (second tokens)) 0)]
    ["y" (point 0 (string->number (second tokens)))]))

; the fold eliminates a line
(define (bend-horizontal pts y)
  (define-values (original shifted)
    (partition (lambda (p) (< (point-y p) y)) pts))
  (define inverted
    (map (lambda (p) (point (point-x p)
                            (invert-value (- (point-y p) (+ y 1))
                                          (- y 1))))
           shifted))
  (sort (remove-duplicates (append original inverted)) sort-point))

(define (bend-vertical pts x)
  (define-values (original shifted)
    (partition (lambda (p) (< (point-x p) x)) pts))
  (define inverted
    (map (lambda (p) (point (invert-value (- (point-x p) (+ x 1))
                                          (- x 1))
                            (point-y p)))
         shifted))
  (sort (remove-duplicates (append original inverted)) sort-point))

(define (bend pts b)
  (if (> (point-x b) 0)
      (bend-vertical pts (point-x b))
      (bend-horizontal pts (point-y b))))

(define (invert-value v width)
  (- width v))

(define (sort-point p1 p2)
    (if (= (point-y p1) (point-y p2))
        (< (point-x p1) (point-x p2))
        (< (point-y p1) (point-y p2))))

(define (max-lst xs)
  (define (max x1 x2)
    (if (> x1 x2) x1 x2))
  (foldl max (first xs) (rest xs)))    

(define (draw-points pts)
  (define mx (max-lst (map point-x pts)))
  (define my (max-lst (map point-y pts)))
  (define get-pt (lambda (x y)
                   (filter (lambda (p) (and (= x (point-x p))
                                            (= y (point-y p))))
                           pts)))
  (for ([y (range 0 (+ my 1))])
    (for ([x (range 0 (+ mx 1))])
      (let ([p (get-pt x y)])
        (if (empty? p) (display ".") (display "#"))))
    (displayln "")))

(define (part-a)
  (define-values (pts-lst bnds-lst) (splitf-at (file->lines "data/DayThirteen.txt")
                                               (lambda (p) (> (string-length p) 0))))
  (define pts (map parse-point pts-lst))
  (define bends (map parse-bend (drop bnds-lst 1)))
  (length (bend pts (first bends))))

(define (part-b)
  (define-values (pts-lst bnds-lst) (splitf-at (file->lines "data/DayThirteen.txt")
                                               (lambda (p) (> (string-length p) 0))))
  (define pts (map parse-point pts-lst))
  (define bends (map parse-bend (drop bnds-lst 1)))
  (define resolved (foldl (lambda (b r) (bend r b)) pts bends))
  (draw-points resolved))

(module+ test
  (require rackunit)

  (for ([input (list "6,10" "0,14" "9,10")]
        [expected (list (point 6 10) (point 0 14) (point 9 10))])
    (check-equal? (parse-point input) expected))

  (for ([input (list "fold along y=7" "fold along x=5")]
        [expected (list (point 0 7) (point 5 0))])
    (check-equal? (parse-bend input) expected))

  (define example-data
    (map parse-point (list "6,10" "0,14" "9,10" "0,3" "10,4" "4,11" "6,0" "6,12" "4,1"
          "0,13" "10,12" "3,4" "3,0" "8,4" "1,10" "2,14" "8,10" "9,0")))

  (define example-result
    (list (point 0 0) (point 2 0) (point 3 0) (point 6 0) (point 9 0)
          (point 0 1) (point 4 1)
          (point 6 2) (point 10 2)
          (point 0 3) (point 4 3)
          (point 1 4) (point 3 4) (point 6 4) (point 8 4) (point 9 4) (point 10 4)))

  (check-equal?
   (bend-horizontal example-data 7)
   example-result)

  (define example-folded-result
    (list (point 0 0) (point 1 0) (point 2 0) (point 3 0) (point 4 0)
          (point 0 1) (point 4 1)
          (point 0 2) (point 4 2)
          (point 0 3) (point 4 3)
          (point 0 4) (point 1 4) (point 2 4) (point 3 4) (point 4 4)))

  (check-equal?
   (bend-vertical (bend-horizontal example-data 7) 5)
   example-folded-result)
          
)
