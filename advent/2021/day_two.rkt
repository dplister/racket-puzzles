#lang racket

(struct move (dir amt))

(struct sub (x y aim))
  
(define (parse-move text)
  (define tokens (string-split text))
  (move (match (first tokens)
          ["forward" 'forward]
          ["up" 'up]
          ["down" 'down])
        (string->number (second tokens))))

(define (get-moves filepath)
  (map parse-move (file->lines filepath)))

(define (action-moves moves location)
  (if (empty? moves) location
      (let ([updated-location
             (match (car moves)
               [(move 'forward amt)
                (sub (+ (sub-x location) amt)
                     (sub-y location)
                     0)]
               [(move 'up amt)
                (sub (sub-x location)
                     (- (sub-y location) amt)
                     0)]
               [(move 'down amt)
                (sub (sub-x location)
                     (+ (sub-y location) amt)
                     0)])])
        (action-moves (rest moves) updated-location))))

(define (action-moves-with-aim moves location)
  (if (empty? moves) location
      (let ([updated-location
             (match (car moves)
               [(move 'forward amt)
                (sub (+ (sub-x location) amt)
                     (+ (sub-y location)
                        (* (sub-aim location) amt))
                     (sub-aim location))]
                [(move 'up amt)
                 (sub (sub-x location)
                      (sub-y location)
                      (- (sub-aim location) amt))]
                [(move 'down amt)
                 (sub (sub-x location)
                      (sub-y location)
                      (+ (sub-aim location) amt))])])
        (action-moves-with-aim (rest moves) updated-location))))

(define (part-a)
  (define result (action-moves (get-moves "data/DayTwo.txt") (sub 0 0 0)))
  (* (sub-x result) (sub-y result)))
  
(define (part-b)
  (define result (action-moves-with-aim (get-moves "data/DayTwo.txt") (sub 0 0 0)))
  (* (sub-x result) (sub-y result)))
