#lang racket

(define (create-map input)
  (create-grid input identity))

(module+ test
  (require rackunit)

  (define example 
    (list 
      "Sabqponm"
      "abcryxxl"
      "accszExk"
      "acctuvwj"
      "abdefghi"))
)
