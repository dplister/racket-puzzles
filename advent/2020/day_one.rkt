#lang racket

(require "../prelude.rkt")

(define (find-total ls amt target)
  (define ss (sort ls <))

  (define (find-combination l n [acc (list)])
    (cond
      [(< (length l) n) #f]
      [(< target (apply + acc)) #f]
      [(and (= n 0) (= target (apply + acc))) acc]
      [(= n 0) #f]
      [else (let ([fc (lambda (v r) (find-combination r (sub1 n) (cons v acc)))])
              (map-with-rest l fc))]))

  (filter number? (flatten (find-combination ss amt))))

(define (expense-report ls amt target)
  (apply * (find-total ls amt target)))

(define (part-a)
  (define input (map string->number (file->lines "data/DayOne.txt")))
  (expense-report input 2 2020))

(define (part-b)
  (define input (map string->number (file->lines "data/DayOne.txt")))
  (expense-report input 3 2020))

(module+ test
  (require rackunit)

  (define example-data 
    (list 1721
          979
          366
          299
          675
          1456))

  (check-equal?
   (expense-report example-data 2 2020)
   514579)

  (check-equal?
   (expense-report example-data 3 2020)
   241861950))
