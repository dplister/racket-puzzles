#lang racket

(require "../prelude.rkt")

(struct player (pos rolls score) #:transparent)

(define (parse-position input)
  ;Player 1 starting position: 10
  (string->number (last (string-split input))))

(define (skip-laps len steps)
  "calculates forward movement once lapping is removed"
  (define laps (quotient steps len))
  (- steps (* laps len)))

(define (move-index len start-index steps)
  (let ([nls (skip-laps len steps)]
        [remaining (- len start-index)])
    (if (> remaining nls)
        (+ start-index nls)
        (- nls remaining))))

(define (generate-roll-n roller n)
  (lambda ()
    (build-list n (lambda (l) (roller)))))

(define (generate-roll size)
  (define counter 1)
  (lambda ()
    (let ([c counter])
      (if (= counter size)
          (set! counter 1)
          (set! counter (add1 counter)))
      c)))

(define (all-rolls n)
  (lambda ()
    (range 1 (+ n 1))))

(define (move path p roller)
  (let* ([rolls (roller)]
         [path-index (move-index (length path) (player-pos p) (foldl + 0 rolls))])
    (player path-index
            (append (player-rolls p) rolls)
            (+ (player-score p) (list-ref path path-index)))))

(define (reached-score? players target)
  (not (empty? (filter (lambda (p) (>= (player-score p) target)) players))))

(define (play-until path players roller [moves 0]
                    #:end-score [end-score #f]
                    #:end-move [end-move #f])
  (if (or (and end-move (= end-move moves))
          (and end-score (reached-score? players end-score)))
      players
      (play-until path
                  (map-until (lambda (p) (move path p roller))
                             players
                             (lambda (p) (and end-score (>= (player-score p) end-score))))
                  roller
                  (add1 moves)
                  #:end-score end-score
                  #:end-move end-move)))

(define (play-quantum path players end-score [moves 0])
  ;(displayln (format "Move: ~v Players: ~v" moves players))
  (if (reached-score? players end-score)
      (index-of players (winner players))
      (let ([nr (next-roller players moves 3)])
        (if (empty? nr)
            (play-quantum path players end-score (add1 moves))
            (map (lambda (r)
                   (play-quantum path
                                 (list-set players
                                           (index-of players (first nr))
                                           (move path (first nr) (lambda () r)))
                                 end-score moves))
                 (perms-dupes (list 1 2 3)))))))

; generate the set of permutations of rolls and create a lambda that returns a set of each 3 for the purposes of sending to (move) - then we can pass each player into a sep play-quantum. This bypasses having to split off after each individual roll and allows us to reuse (move).
                       
; so if move 0 everyone should have 0 * expected-rolls (0) rolls
; move 1 has 1 * expected-rolls (3) rolls
(define (next-roller players move expected-rolls)
  "finds the set of players in the list that have less than the expected amount of rolls"
  (filter (lambda (p) (< (length (player-rolls p)) (* move expected-rolls))) players))
        
(define (total-rolls ps)
  (foldl (lambda (p r) (+ (length (player-rolls p)) r)) 0 ps))

(define (loser ps)
  (first (sort ps < #:key player-score)))

(define (winner ps)
  (first (sort ps > #:key player-score)))

(define (part-a)
  (define players (map (lambda (p) (player (sub1 (parse-position p)) (list) 0)) (file->lines "data/DayTwentyOne.txt")))
  (play-game players))

(define (play-game players)
  (define end-players (play-until (range 1 11) players (generate-roll-n (generate-roll 100) 3) #:end-score 1000))
  ;(displayln (format "Score ~v Total Rolls ~v" (player-score (loser end-players)) (total-rolls end-players)))
  (* (player-score (loser end-players)) (total-rolls end-players)))

(define (play-quantum-game players)
  (define scores (play-quantum (range 1 11) players 21))
  (define ds (count-distinct scores))
  (displayln "Scores")
  (for ([s (dict-keys ds)])
    (displayln (format "P: ~v Score: ~v" s (dict-ref ds s))))
  ds)

; not 428736

(module+ test
  (require rackunit)

  (define test-path
    (range 1 11))

  (define test-path-len
    (length test-path))

  (for ([input (list 3 23 11 24)]
        [expected (list 3 3 1 4)])
    (check-equal?
     (skip-laps test-path-len input)
     expected))

  (for ([input (list (list 1 2)
                     (list 8 1)
                     (list 9 10)
                     (list 5 12)
                     (list 5 15))]
        [expected (list 3 9 9 7 0)])
    (check-equal?
     (move-index test-path-len (first input) (second input))
     expected))

  (check-equal?
   (move test-path (player 1 (list) 0) (lambda () (list 1 2 3)))
   (player 7 (list 1 2 3) 8))

  (check-equal?
   (move test-path (player 6 (list) 11) (lambda () (list 0 0 22)))
   (player 8 (list 0 0 22) 20))

  (let ([ps (play-until test-path
                        (list (player 3 (list) 0)
                              (player 7 (list) 0))
                        (generate-roll-n (generate-roll 100) 3)
                        #:end-move 1)])
    (check-equal? ps (list (player 9 (list 1 2 3) 10) (player 2 (list 4 5 6) 3)))
    (check-equal? (total-rolls ps) 6))

  (let ([ps (play-until test-path
                        (list (player 3 (list) 0)
                              (player 7 (list) 0))
                        (generate-roll-n (generate-roll 100) 3)
                        #:end-move 4)])
    (check-equal? (player-score (first ps)) 26)
    (check-equal? (player-pos (first ps)) 5)
    (check-equal? (player-score (second ps)) 22)
    (check-equal? (player-pos (second ps)) 5)
    (check-equal? (total-rolls ps) 24))

  (let ([ps (play-until test-path
                               (list (player 3 (list) 0)
                                     (player 7 (list) 0))
                               (generate-roll-n (generate-roll 100) 3)
                               #:end-score 1000)])
    (check-equal? (player-score (first ps)) 1000)
    (check-equal? (player-pos (first ps)) 9)
    (check-equal? (player-score (second ps)) 745)
    (check-equal? (player-pos (second ps)) 2)
    (check-equal? (total-rolls ps) 993))

  (check-equal?
   (loser (list (player 0 (list) 12)
                (player 0 (list) 24)))
   (player 0 (list) 12))
  (check-equal?
   (loser (list (player 0 (list) 24)
                (player 0 (list) 2)))
   (player 0 (list) 2))

  (check-equal?
   (play-game (list (player 3 (list) 0) (player 7 (list) 0)))
   739785)

  (let ([qg (play-quantum-game (list (player 3 (list) 0)
                                     (player 7 (list) 0)))])
    (check-equal? (dict-ref qg 0) 444356092776315)
    (check-equal? (dict-ref qg 1) 341960390180808))
)
