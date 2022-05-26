#lang racket

(define (nearest num step)
  "finds the first step greater than or equal to num"
  (define n (* step (quotient num step)))
  (or (and (>= n num) n)
      (+ n step)))

(define (find-nearest num steps)
  "finds the step that occurs nearest the num"
  (first (sort (map (lambda (s) (cons s (nearest num s))) steps) < #:key cdr)))

(define (in-sequence steps)
  "finds a sequence where each step occurs 1 min after the rest"
  (define indexed (filter-map (lambda (i v) (and (not (equal? v "x"))
                                                 (cons i v)))
                              (range 0 (length steps))
                              steps))
  (define (loop ls ts period)
    (cond
      [(empty? ls) ts]
      [(not (equal? (modulo (+ ts (car (first ls)))
                            (cdr (first ls)))
                    0))
       (loop ls (+ ts period) period)]
      [else (loop (rest ls) ts (lcm period (cdr (first ls))))]))
  (loop indexed (cdr (first indexed)) (cdr (first indexed))))

(define (part-a)
  (define lines (file->lines "data/DayThirteen.txt"))
  (define start-time (string->number (first lines)))
  (define result (find-nearest start-time
                               (filter-map string->number (string-split (second lines) ","))))
  (* (car result) (- (cdr result) start-time)))

(define (part-b)
  (define lines (file->lines "data/DayThirteen.txt"))
  (in-sequence (map (lambda (v) (or (string->number v)
                                    "x"))
                    (string-split (second lines) ","))))

(module+ test
  (require rackunit)

  (define example-times
    (list 7 13 59 31 19))

  (for ([t example-times]
        [expected (list 945 949 944 961 950)])
    (check-equal?
     (nearest 939 t)
     expected))

  (check-equal?
   (find-nearest 939 example-times)
   '(59 . 944))

  (for ([input (list (list 17 "x" 13 19)
                     (list 67 7 59 61)
                     (list 67 "x" 7 59 61)
                     (list 67 7 "x" 59 61)
                     (list 1789 37 47 1889))]
        [expected (list 3417 754018 779210 1261476 1202161486)])
    (check-equal?
     (in-sequence input)
     expected))
    
)
