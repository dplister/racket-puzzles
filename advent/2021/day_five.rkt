#lang racket

(struct point (x y) #:transparent)

(define (points-between p1 p2)
  (if (and (equal? (point-x p1) (point-x p2))
           (equal? (point-y p1) (point-y p2)))
      (list p1)
      (cons p1
            (points-between
             (point
              (equal-or-increment (point-x p1) (point-x p2))
              (equal-or-increment (point-y p1) (point-y p2)))
             p2))))

(define (equal-or-increment n1 n2)
  (cond
    [(= n1 n2) n1]
    [(< n1 n2) (+ n1 1)]
    [(> n1 n2) (- n1 1)]))

(define (parse-dir line)
  (define pts (string-split line))
  (list
   (parse-point (first pts))
   (parse-point (third pts))))

(define (parse-point input)
  (define nums (get-comma-numbers input))
  (point (first nums) (second nums)))

(define (get-comma-numbers line)
  (map string->number (string-split line ",")))

(define (combine-points pts)
  (define h (make-hash))
  (for ([p pts])
    (dict-update! h p add1 0))
  h)

(define (common-point p1 p2)
  (or (= (point-x p1) (point-x p2))
      (= (point-y p1) (point-y p2))))

(define (get-orthogonals dirs)
  (filter (lambda (d) (common-point (first d) (second d))) dirs))

(define (get-all-points dirs)
  (flatten (map
            (lambda (pts) (points-between (first pts) (second pts)))
            dirs)))

(define (count-overlaps dirs)
  (define all-pts (get-all-points dirs))
  (define combs (combine-points all-pts))
  (length (filter (lambda (v) (> v 1)) (dict-values combs))))

(define (part-a)
  (define lines (file->lines "data/DayFive.txt"))
  (define dirs (map parse-dir lines))
  (define valid-dirs (get-orthogonals dirs))
  (count-overlaps valid-dirs))

(define (part-b)
  (define lines (file->lines "data/DayFive.txt"))
  (define dirs (map parse-dir lines))
  (count-overlaps dirs))

(module+ test
  (require rackunit)

  (check-equal?
   (list (point 1 1) (point 1 2) (point 1 3))
   (points-between (point 1 1) (point 1 3)))

  (check-equal?
   (parse-dir "1,2 -> 34,56")
   (list (point 1 2) (point 34 56)))

  (let ([h (combine-points (list (point 1 2) (point 2 3) (point 1 2)))])
    (check-equal? 2 (dict-ref h (point 1 2)))
    (check-equal? 1 (dict-ref h (point 2 3))))

  (define example-dirs
    (map parse-dir
         (list "0,9 -> 5,9"
               "8,0 -> 0,8"
               "9,4 -> 3,4"
               "2,2 -> 2,1"
               "7,0 -> 7,4"
               "6,4 -> 2,0"
               "0,9 -> 2,9"
               "3,4 -> 1,4"
               "0,0 -> 8,8"
               "5,5 -> 8,2")))

  (check-equal? (get-orthogonals example-dirs)
                (list
                 (list (point 0 9) (point 5 9))
                 (list (point 9 4) (point 3 4))
                 (list (point 2 2) (point 2 1))
                 (list (point 7 0) (point 7 4))
                 (list (point 0 9) (point 2 9))
                 (list (point 3 4) (point 1 4))))

  (check-equal? (list
                 (point 0 9)
                 (point 1 9)
                 (point 2 9)
                 (point 3 9)
                 (point 4 9)
                 (point 5 9)
                 (point 9 4)
                 (point 8 4)
                 (point 7 4)
                 (point 6 4)
                 (point 5 4)
                 (point 4 4)
                 (point 3 4)
                 (point 2 2)
                 (point 2 1)
                 (point 7 0)
                 (point 7 1)
                 (point 7 2)
                 (point 7 3)
                 (point 7 4)
                 (point 0 9)
                 (point 1 9)
                 (point 2 9)
                 (point 3 4)
                 (point 2 4)
                 (point 1 4))
                (get-all-points (get-orthogonals example-dirs)))

  (check-equal? (count-overlaps (get-orthogonals example-dirs)) 5)

  (check-equal? (points-between (point 1 1) (point 3 3))
                (list (point 1 1) (point 2 2) (point 3 3)))

  (check-equal? (points-between (point 9 7) (point 7 9))
                (list (point 9 7) (point 8 8) (point 7 9)))

  (check-equal? (count-overlaps example-dirs) 12)
  )
