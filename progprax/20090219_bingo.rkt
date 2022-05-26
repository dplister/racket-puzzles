#lang racket

(define (create-bingo [size 5] [rng 15])
  (map (lambda (start)
         (map (lambda (h) (random start (+ start rng)))
              (make-list size start)))
       (range 1 (* size rng) rng)))

(module+ test
  (require rackunit)

  (let ([b (create-bingo 5 15)])
    (for ([v '((1 . 15) (16 . 30) (31 . 45) (46 . 60))]
          [row b])
      (check-equal? (andmap (lambda (cell) (and (>= cell (car v)) (<= cell (cdr v)))) row)
                    #t (format "row: ~v range: ~v" row v))))
)
