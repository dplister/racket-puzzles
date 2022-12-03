#lang racket

(define scores (hash #\A 1 #\B 2 #\C 3))
(define winning (hash #\A #\B #\B #\C #\C #\A))
(define losing (hash #\A #\C #\B #\A #\C #\B))
(define moves (hash #\X #\A #\Y #\B #\Z #\C))

(define (normalise-move m)
  (hash-ref moves m m))

(define (battle opponent you)
  "calculates combat score as result of opponent and your action"
  (cond
    [(equal? opponent you) 3]
    [(equal? (hash-ref winning opponent) you) 6]
    [else 0]))

(define (score opponent you)
  "calculates total score of round"
  (define your-move (normalise-move you))
  (+ (battle opponent your-move) (hash-ref scores your-move)))

(define (split-line input)
  "converts line into a pair of numbers"
  (map (lambda (v) (string-ref v 0)) (string-split input)))

(define (process-input)
  (map split-line (file->lines "day_two.txt")))

(define (opposing-move opponent expected-result)
  "generates move in response to opponent based on expected-result"
  (match expected-result
    [#\X (hash-ref losing opponent)]
    [#\Y opponent]
    [#\Z (hash-ref winning opponent)]))
  
(define (rigged-score opponent expected-result)
  "calculates total score of round based on expected result"
  (define my-move (opposing-move opponent expected-result))
  (score opponent my-move))

(define (part-a)
  (apply + (map (lambda (l) (apply score l)) (process-input))))

(define (part-b)
  (apply + (map (lambda (l) (apply rigged-score l)) (process-input))))

(part-b)


(module+ test
  (require rackunit)

  ; part a

  (check-equal? (battle #\A #\B) 6)
  (check-equal? (battle #\B #\A) 0)
  (check-equal? (battle #\C #\C) 3)

  (check-equal? (score #\A #\Y) 8)
  (check-equal? (score #\B #\X) 1)
  (check-equal? (score #\C #\Z) 6)

  (check-equal? (split-line "A C") (list #\A #\C))

  ; part b
  
  (check-equal? (rigged-score #\A #\Y) 4)
  (check-equal? (rigged-score #\B #\X) 1)
  (check-equal? (rigged-score #\C #\Z) 7)
  
  
)
