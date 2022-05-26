#lang racket

(require "prelude.rkt")

(struct point (x y) #:transparent)

(define (parse-trench input)
  (define tokens (rest (regexp-match #rx"x=([0-9]+)..([0-9]+), y=([\\-]?[0-9]+)..([\\-]?[0-9]+)" input)))
  (list (point (string->number (first tokens))
               (string->number (fourth tokens)))
        (point (string->number (second tokens))
               (string->number (third tokens)))))

(define (step position velocity)
  (values (point (+ (point-x position) (point-x velocity))
                 (+ (point-y position) (point-y velocity)))
          (point (approach-zero (point-x velocity))
                 (- (point-y velocity) 1))))

(define (execute-steps-until positions velocities end-steps?)
  (if (end-steps? positions velocities)
      (values positions velocities)
      (let-values ([(p v) (step (first positions) (first velocities))])
        (execute-steps-until (cons p positions) (cons v velocities) end-steps?))))

(define (steps-counted-f amt)
  (lambda (ps vs) (= (sub1 (length ps)) amt))) ; the set of steps includes the origin, hence -1

(define (cross-trench-f t)
  (lambda (ps vs) (< (point-y (first ps)) (point-y t))))

(define (cross-trench starting-p starting-velocity trench)
  (define-values (ps vs)
    (execute-steps-until (list starting-p) (list starting-velocity)
                         (cross-trench-f (second trench))))
  (define matches (inside-trench ps trench))
  (if (empty? matches)
      #f
      (last matches)))

(define (find-highest trench)
  (define max-x (point-x (last trench)))
  (define max-y (abs (point-y (last trench))))
  (define highest-y 0)
  (define highest-starting-p (point 0 0))
  (define stop-search (cross-trench-f (second trench)))
  (define origin (point 0 0))
  (for ([y (range 0 (+ max-y 1))])
    (for ([x (range 0 (+ max-x 1))])
      (let-values ([(ps vs) (execute-steps-until (list origin) (list (point x y)) stop-search)])
        (let ([matches (inside-trench ps trench)])
          (when (not (empty? matches))
            (let ([my (apply max (map (lambda (p) (point-y p)) ps))])
              (when (> my highest-y)
                (set! highest-y my)
                (set! highest-starting-p (point x y)))))))))
  (values highest-y highest-starting-p))

(define (part-a)
  (define input (string-trim (file->string "data/DaySeventeen.txt")))
  (define trench (parse-trench input))
  (define-values (hy hp) (find-highest trench))
  hy)

(define (part-b)
  (define input (string-trim (file->string "data/DaySeventeen.txt")))
  (define trench (parse-trench input))
  (length (find-all trench)))

(define (find-all-inside x y trench stop-search [acc (list)])
  (if (< y (point-y (last trench))) acc
      (let-values ([(ps vs) (execute-steps-until (list (point 0 0)) (list (point x y)) stop-search)])
        (find-all-inside (if (= x (point-x (last trench))) 0 (+ x 1))
                         (if (= x (point-x (last trench))) (- y 1) y)
                         trench
                         stop-search
                         (if (not (empty? (inside-trench ps trench)))
                             (cons (point x y) acc)
                             acc)))))

(define (find-all trench)
  (define stop-search (cross-trench-f (second trench)))
  (define starting-x 0)
  (define starting-y (* -1 (point-y (second trench))))
  (find-all-inside starting-x starting-y trench stop-search))

(define (inside-trench positions trench)
  (filter (lambda (p)
            (and
             ; x is pos
             (>= (point-x p) (point-x (first trench)))
             (<= (point-x p) (point-x (second trench)))
             ; y is neg
             (>= (point-y p) (point-y (second trench)))
             (<= (point-y p) (point-y (first trench)))))
          positions))

(define (approach-zero v)
  (cond
    [(< v 0) (add1 v)]
    [(> v 0) (sub1 v)]
    [else v]))

(module+ test
  (require rackunit)

  (define test-trench "target area: x=20..30, y=-10..-5")

  (check-equal?
   (parse-trench test-trench)
   (list (point 20 -5)
         (point 30 -10)))

  (let-values ([(ps vs)
                (execute-steps-until (list (point 0 0))
                                     (list (point 7 2))
                                     (steps-counted-f 2))])
    (check-equal? (length ps) 3)
    (check-equal? (length vs) 3)
    (check-equal? (point-x (second ps)) 7)
    (check-equal? (point-y (second ps)) 2)
    (check-equal? (point-x (first ps)) 13)
    (check-equal? (point-y (first ps)) 3))

  (let ([p (cross-trench (point 0 0) (point 7 2) (parse-trench test-trench))])
    (check-equal? p (point 28 -7)))

  (let ([p (cross-trench (point 0 0) (point 6 3) (parse-trench test-trench))])
    (check-equal? p (point 21 -9)))

  (let ([p (cross-trench (point 0 0) (point 9 0) (parse-trench test-trench))])
    (check-equal? p (point 30 -6)))

  (let ([p (cross-trench (point 0 0) (point 17 -4) (parse-trench test-trench))])
    (check-equal? p #f))

  (let-values ([(hy hp) (find-highest (parse-trench test-trench))])
    (check-equal? hy 45)
    (check-equal? hp (point 6 9)))

  (define example-points
     (list
      (point 23 -10) (point 25 -9) (point 27 -5) (point 29 -6)  (point 22 -6)  (point 21 -7)  (point 9 0)   (point 27 -7)  (point 24 -5)
      (point 25 -7) (point 26 -6) (point 25 -5) (point 6 8)  (point 11 -2) (point 20 -5) (point 29 -10) (point 6 3)  (point 28 -7)
      (point 8 0) (point 30 -6) (point 29 -8) (point 20 -10) (point 6 7) (point 6 4) (point 6 1) (point 14 -4) (point 21 -6)
      (point 26 -10) (point 7 -1) (point 7 7) (point 8 -1) (point 21 -9) (point 6 2) (point 20 -7) (point 30 -10) (point 14 -3)
      (point 20 -8) (point 13 -2) (point 7 3) (point 28 -8) (point 29 -9) (point 15 -3) (point 22 -5) (point 26 -8) (point 25 -8)
      (point 25 -6) (point 15 -4) (point 9 -2) (point 15 -2) (point 12 -2) (point 28 -9) (point 12 -3) (point 24 -6) (point 23 -7)
      (point 25 -10) (point 7 8)  (point 11 -3) (point 26 -7) (point 7 1)  (point 23 -9) (point 6 0)  (point 22 -10) (point 27 -6)
      (point 8 1) (point 22 -8) (point 13 -4) (point 7 6) (point 28 -6) (point 11 -4) (point 12 -4) (point 26 -9) (point 7 4)
      (point 24 -10) (point 23 -8) (point 30 -8) (point 7 0)  (point 9 -1) (point 10 -1) (point 26 -5) (point 22 -9) (point 6 5)
      (point 7 5) (point 23 -6) (point 28 -10) (point 10 -2) (point 11 -1) (point 20 -9) (point 14 -2) (point 29 -7) (point 13 -3)
      (point 23 -5) (point 24 -8) (point 27 -9) (point 30 -7) (point 28 -5) (point 21 -10) (point 7 9)  (point 6 6)  (point 21 -5)
      (point 27 -10) (point 7 2)  (point 30 -9) (point 21 -8) (point 22 -7) (point 24 -9) (point 20 -6) (point 6 9)  (point 29 -5)
      (point 8 -2) (point 27 -8) (point 30 -5) (point 24 -7)))

  (let* ([t (parse-trench test-trench)]
         [ap (find-all t)])
    (check-equal?
     (difference ap example-points)
     (list)))
)

