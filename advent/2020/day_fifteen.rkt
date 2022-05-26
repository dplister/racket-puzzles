#lang racket

(define (next hs prev)
  (let ([v (hash-ref hs prev)])
    (if (= (length v) 1) 0
        (- (first v) (second v)))))

(define (guess hs turn prev max-turns)
  (define last-guess prev)
  (for ([t (range turn (add1 max-turns))])
    ;(display (format "turn ~v prev ~v" t last-guess))
    (set! last-guess (next hs last-guess))
    ;(displayln (format "last ~v" last-guess))
    (hash-set! hs last-guess
               (cons t
                     (hash-ref hs last-guess (list)))))
  last-guess)
  
; cons the turn onto the start of value for key (guess)

(define (preload-guesses ls)
  (define hs (make-hash))
  (for ([g ls]
        [i (range 1 (add1 (length ls)))])
    (hash-set! hs g (cons i (first-list (hash-ref hs g (list))))))
  hs)

(define (first-list ls)
  (if (or (not ls)
          (empty? ls))
      (list)
      (take ls 1)))

(define (part-a)
  (define input (list 2 0 1 7 4 14 18))
  (guess (preload-guesses input)
         (add1 (length input))
         (last input)
         2020))

(define (part-b)
  (define input (list 2 0 1 7 4 14 18))
  (guess (preload-guesses input)
         (add1 (length input))
         (last input)
         30000000))

(module+ test
  (require rackunit)

  (define (starting-values)
    (preload-guesses (list 0 3 6)))

  (check-equal?
   (hash->list (starting-values))
   '((0 . (1)) (3 . (2)) (6 . (3))))

  (check-equal?
   (next (starting-values) 6)
   0)

  (check-equal?
   (guess (starting-values) 4 6 4)
   0)

  (for ([turn '((4 . 0) (5 . 3) (6 . 3) (7 . 1) (8 . 0) (9 . 4) (10 . 0) (2020 . 436))])
    (check-equal?
     (guess (starting-values) 4 6 (car turn))
     (cdr turn)))

  (for ([turn '(((1 3 2) . 1) ((2 1 3) . 10) ((1 2 3) . 27) ((2 3 1) . 78) ((3 2 1) . 438) ((3 1 2) . 1836))])
    (check-equal?
     (guess (preload-guesses (car turn))
            (add1 (length (car turn)))
            (last (car turn))
            2020)
     (cdr turn)))

  ;(for ([turn '(((0 3 6) . 175594) ((1 3 2) . 2578) ((2 1 3) . 3544142) ((1 2 3) . 261214) ((2 3 1) . 6895259) ((3 2 1) . 18) ((3 1 2) . 362))])
  ;  (check-equal?
  ;   (guess (preload-guesses (car turn))
  ;          (add1 (length (car turn)))
  ;          (last (car turn))
  ;          30000000)
  ;   (cdr turn)))

    
)
