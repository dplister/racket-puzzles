#lang racket

(provide (all-defined-out))

(require "../prelude.rkt")

(define (distance-counts ls [ks #hash()])
  (if (< (length ls) 2) ks
      (distance-counts (rest ls)
                       (dict-update ks (distance (first ls) (second ls)) add1 0))))

(define (joltages ls)
  (append (cons 0 (sort ls <)) (list (+ (apply max ls) 3))))

(define (part-a)
  (define ls (map string->number (file->lines "data/DayTen.txt")))
  (define mps (distance-counts (joltages ls)))
  (* (dict-ref mps 1) (dict-ref mps 3)))

(define (take-near ls target)
  "takes all values from start of list that are <= target"
  (takef ls (lambda (v) (<= v target))))

(define (find-all-combos jolts)
  (define (loop ls ds)
    (if (empty? ls)
        (dict-ref ds (last jolts))
        (let ([v (first ls)])
          (loop (rest ls)
                (dict-set ds v (+ (dict-ref ds (- v 3) 0) (dict-ref ds (- v 2) 0) (dict-ref ds (- v 1) 0)))))))
  (loop (rest jolts) (dict-set #hash() 0 1)))

(define (part-b)
  (define ls (map string->number (file->lines "data/DayTen.txt")))
  (find-all-combos (joltages ls)))

(module+ test
  (require rackunit)

  (check-equal?
   (distance-counts (list 1 2 3))
   #hash((1 . 2)))
  (check-equal?
   (distance-counts (list 1 4 5))
   #hash((3 . 1) (1 . 1)))

  (check-equal?
   (joltages (list 1 2 3))
   (list 0 1 2 3 6))

  (define example-data
    (list 16 10 15 5 1 11 7 19 6 12 4))

  (check-equal?
   (distance-counts (joltages example-data))
   #hash((1 . 7) (3 . 5)))

  (define example-data-longer
    (list 28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))

  (check-equal?
   (distance-counts (joltages example-data-longer))
   #hash((1 . 22) (3 . 10)))

  (check-equal?
   (find-all-combos (joltages example-data))
   8)

  (check-equal?
   (find-all-combos (joltages example-data-longer))
   19208)
   
)
