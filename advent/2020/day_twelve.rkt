#lang racket


(define orientations
  '((#\N . (0 -1))
    (#\E . (1 0))
    (#\S . (0 1))
    (#\W . (-1 0))))

(define (rotate current r)
  (define (loop o n)
    (if (= n 0)
        (list-ref orientations o)
        (cond
          [(< n 0)
           (loop (if (= o 0)
                     (sub1 (length orientations))
                     (sub1 o))
                 (add1 n))]
          [else
           (loop (if (= o (sub1 (length orientations)))
                     0
                     (add1 o))
                 (sub1 n))])
        ))
  (loop (index-of orientations current)
        (quotient r 90)))

(define (rotate-wp pos dir degrees)
  (if (<= degrees 0) pos
  (match dir
    [#\L (rotate-wp (list (second pos)
                          (* (first pos) -1))
                    dir
                    (- degrees 90))]
    [#\R (rotate-wp (list (* (second pos) -1)
                          (first pos))
                    dir
                    (- degrees 90))])))

(define (translate pos step amt)
  (map (lambda (p s) (+ p (* s amt))) pos step))

(define (parse-dir input)
  (list
   (string-ref input 0)
   (string->number (substring input 1))))

(define (manhattan-distance pos)
  (+ (abs (first pos)) (abs (second pos))))

(define (move dir pos)
  (match dir
    [(list #\L rot)
     (struct-copy position pos
                  [orient (rotate (position-orient pos) (* -1 rot))])]
    [(list #\R rot)
     (struct-copy position pos
                  [orient (rotate (position-orient pos) rot)])]
    [(list #\F amt)
     (struct-copy position pos
                  [coord (translate (position-coord pos)
                                    (cdr (position-orient pos))
                                    amt)])]
    [(list dir amt)
     (struct-copy position pos
                  [coord (translate (position-coord pos)
                                    (cdr (findf (lambda (v) (equal? dir (car v))) orientations))
                                    amt)])]))

(define (move-waypoint dir pos)
  (match dir
    [(list dir rot) ; L R
     #:when (or (equal? dir #\L) (equal? dir #\R))
     (rotate-wp pos dir rot)]
    [(list dir amt) ; N S W E
     (translate pos
                (cdr (findf (lambda (v) (equal? dir (car v))) orientations))
                amt)]))

(define (move-sway dir poses) ; ship, waypoint
  (match dir
    [(list #\F amt)
     (list
      (translate (first poses)
                 (second poses)
                 amt)
      (second poses))]
    [else
     (list
      (first poses)
      (move-waypoint dir (second poses)))]))

(define (apply-moves dirs pos)
  (foldl (lambda (l r) (move l r)) pos dirs))

(define (apply-moves-waypoint dirs poses)
  (foldl (lambda (l r) (move-sway l r)) poses dirs))

(define (part-a)
  (define input (map parse-dir (file->lines "data/DayTwelve.txt")))
  (define starting
    (position (list 0 0) (second orientations)))
  (manhattan-distance (position-coord (apply-moves input starting))))

(define (part-b)
  (define input (map parse-dir (file->lines "data/DayTwelve.txt")))
  (define starting
    (list (list 0 0) (list 10 -1)))
  (manhattan-distance (first (apply-moves-waypoint input starting))))

(module+ test
  (require rackunit)

  (check-equal?
   (car (rotate (first orientations) -90))
   #\W)
  (check-equal?
   (car (rotate (second orientations) 90))
   #\S)
  (check-equal?
   (car (rotate (second orientations) 180))
   #\W)

  (check-equal?
   (manhattan-distance (list 17 8))
   25)

  (define starting
    (position (list 0 0) (second orientations)))
  
  (check-equal?
   (move (list #\N 10)
         starting)
   (position (list 0 -10)
             (second orientations)))

  (check-equal?
   (move (list #\L 90)
         starting)
   (position (list 0 0)
             (first orientations)))

  (check-equal?
   (move (list #\F 2)
         starting)
   (position (list 2 0)
             (second orientations)))


  (define example-dirs
    (map parse-dir (list "F10"
                         "N3"
                         "F7"
                         "R90"
                         "F11")))
  (check-equal?
   (apply-moves example-dirs starting)
   (position (list 17 8)
             (third orientations)))

  (check-equal?
   (move-sway (first example-dirs) (list (list 0 0) (list 10 -1)))
   '((100 -10) (10 -1)))

  (check-equal?
   (apply-moves-waypoint (take example-dirs 1) (list (list 0 0) (list 10 -1)))
   '((100 -10) (10 -1)))

  (check-equal?
   (apply-moves-waypoint (take example-dirs 2) (list (list 0 0) (list 10 -1)))
   '((100 -10) (10 -4)))

  (check-equal?
   (apply-moves-waypoint (take example-dirs 3) (list (list 0 0) (list 10 -1)))
   '((170 -38) (10 -4)))

  (check-equal?
   (apply-moves-waypoint (take example-dirs 4) (list (list 0 0) (list 10 -1)))
   '((170 -38) (4 10)))

  (check-equal?
   (apply-moves-waypoint example-dirs (list (list 0 0) (list 10 -1)))
   '((214 72) (4 10)))

   
)
