#lang racket

(struct point (x y [z #:mutable] [flashed #:mutable]) #:transparent)

(define (string->numbers line)
  (map parse-number (string->list line)))

(define (parse-number c)
  (match c
    [#\1 1] [#\2 2] [#\3 3] [#\4 4] [#\5 5]
    [#\6 6] [#\7 7] [#\8 8] [#\9 9] [#\0 0]))

(define (parse-coords lst)
   (flatten
    (for/list ([y (range 0 (length lst))]
               [l lst])
      (for/list ([x (range 0 (string-length l))]
                 [c (string->numbers l)])
        (point x y c #f)))))

(define (get-coord ls x y)
  (define pt (filter (lambda (p) (and (equal? (point-x p) x)
                                      (equal? (point-y p) y)))
                     ls))
  (if (empty? pt) 'nil (first pt)))
  

(define (surrounding ls pt)
  (define x (point-x pt))
  (define y (point-y pt))
  (filter point?
          (map (lambda (p) (get-coord ls (first p) (second p))) 
               (list (list (- x 1) y) ; left
                     (list (+ x 1) y) ; right
                     (list (- x 1) (- y 1)) ; upper-left
                     (list (- x 1) (+ y 1)) ; lower-left
                     (list (+ x 1) (- y 1)) ; upper-right
                     (list (+ x 1) (+ y 1)) ; lower-right
                     (list x (- y 1)) ; up
                     (list x (+ y 1)))))) ; down

(define (incz pt)
  (set-point-z! pt (+ 1 (point-z pt)))
  pt)

(define (flash? pt)
  (and (> (point-z pt) 9)
       (not (point-flashed pt))))

(define (do-flash ls source)
  (set-point-flashed! source #t)
  (for ([s (surrounding ls source)]) (incz s)))

(define (step ls)
  (for ([pt ls])
    (incz pt))
  (define total (trigger-flashes ls))
  (zero-flashed ls)
  (values total ls))

(define (run-until ls f? [prev-total 0])
  (if (f? prev-total ls) ls
      (let-values ([(t l) (step ls)])
        (run-until ls f? t))))

(define (zero-flashed ls)
  (for ([l ls])
    (when (point-flashed l)
        (set-point-flashed! l #f)
        (set-point-z! l 0))))

(define (trigger-flashes ls [total-flashes 0])
  (define ready (filter flash? ls))
  (cond
    [(empty? ready) total-flashes]
    [else
     (for ([r ready])
       (do-flash ls r))
     (trigger-flashes ls (+ total-flashes (length ready)))]))

(define (part-a)
  (define coords (parse-coords (file->lines "data/DayEleven.txt")))
  (define run-counter 0)
  (define flash-total 0)
  (define continue? 
    (lambda (current-total current-ls)
      (set! flash-total (+ flash-total current-total))
      (let ([current-counter run-counter])
        (set! run-counter (+ 1 run-counter))
        (= current-counter 100))))
  (define result (run-until coords continue?))
  flash-total)

(define (part-b)
  (define coords (parse-coords (file->lines "data/DayEleven.txt")))
  (define run-counter 0)
  (define continue? 
    (lambda (current-total current-ls)
      (cond
        [(= current-total (length current-ls)) #t]
        [else
         (set! run-counter (+ 1 run-counter))
         #f])))
  (define result (run-until coords continue?))
  run-counter)

(module+ test
  (require rackunit)

  (define small-example-steps
    (list (parse-coords (list "11111" "19991" "19191" "19991" "11111"))
          (parse-coords (list "34543" "40004" "50005" "40004" "34543"))
          (parse-coords (list "45654" "51115" "61116" "51115" "45654"))))

  (for ([i (range 0 (- (length small-example-steps) 1))])
    (let-values ([(fs ls) (step (list-ref small-example-steps i))])
      (check-equal?
       ls
       (list-ref small-example-steps (+ i 1)))))

  (define (large-example-steps) ; just the 10 step ones
    (list (parse-coords (list "5483143223"
                              "2745854711"
                              "5264556173"
                              "6141336146"
                              "6357385478"
                              "4167524645"
                              "2176841721"
                              "6882881134"
                              "4846848554"
                              "5283751526"))
          (parse-coords (list "0481112976"
                              "0031112009"
                              "0041112504"
                              "0081111406"
                              "0099111306"
                              "0093511233"
                              "0442361130"
                              "5532252350"
                              "0532250600"
                              "0032240000"))))

  (let* ([run-counter 0]
         [flash-total 0]
         [continue? 
          (lambda (current-total current-ls)
            (set! flash-total (+ flash-total current-total))
            (let ([current-counter run-counter])
              (set! run-counter (+ 1 run-counter))
              (= current-counter 10)))]
         [l (run-until (first (large-example-steps)) continue?)])
    (check-equal? flash-total 204)
    (check-equal? l (second (large-example-steps))))

  (let* ([run-counter 0]
         [continue? 
          (lambda (current-total current-ls)
            (cond
              [(= current-total (length current-ls)) #t]
              [else
               (set! run-counter (+ 1 run-counter))
               #f]))]
         [l (run-until (first (large-example-steps)) continue?)])
    (check-equal? run-counter 195)
    (check-equal? l (parse-coords (list "0000000000"
                                        "0000000000"
                                        "0000000000"
                                        "0000000000"
                                        "0000000000"
                                        "0000000000"
                                        "0000000000"
                                        "0000000000"
                                        "0000000000"
                                        "0000000000"))))
)
