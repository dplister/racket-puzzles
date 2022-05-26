#lang racket

(require "../prelude.rkt")

(struct ticket (rules mine others) #:transparent)

(define (parse-rule input)
  (define tokens (drop (regexp-match #rx"([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" input) 1))
  (define numbers (map string->number (drop tokens 1)))
  (list (first tokens)
        (cons (first numbers) (second numbers))
        (cons (third numbers) (fourth numbers))))

(define (parse-line input)
  (map string->number (string-split input ",")))

(define (parse-input input)
  (define sets (sublists input (lambda (v) (or (equal? "your ticket:" v)
                                               (equal? "nearby tickets:" v)))
                         #:include-break #f))
  (ticket
   (map parse-rule (filter (compose1 not string-empty?) (first sets)))
   (first (map parse-line (filter (compose1 not string-empty?) (second sets))))
   (map parse-line (third sets))))

(define (any-invalid-rule rules tkt)
  "returns the set of numbers that don't match any rule"
  (filter-map (lambda (v) (if (= (length (matching-rules v rules)) 0) v #f)) tkt))

(define (match-rule? value rule)
  "determines if the rule is valid for value"
  (or (and (>= value (car (second rule)))
           (<= value (cdr (second rule))))
      (and (>= value (car (third rule)))
           (<= value (cdr (third rule))))))

(define (matching-rules v rules)
  "finds the set of rules that would determine v to be a valid number"
  (filter (lambda (r)
            (match-rule? v r))
          rules))

(define (match-rule-all? ls rule)
  (if (empty? ls) #t
      (and (match-rule? (first ls) rule)
           (match-rule-all? (rest ls) rule))))

(define (all-invalid-rules state)
  (append-map
   (lambda (t)
     (any-invalid-rule (ticket-rules state) t))
   (ticket-others state)))

(define (only-valid-tickets state)
  (filter (lambda (t) (empty? (any-invalid-rule (ticket-rules state) t)))
          (ticket-others state)))

(define (rule-values rules vals matches?)
  "returns all vals that have a corresponding rule that matches"
  (cond
    [(empty? rules) (list)]
    [(matches? (first (first rules)))
     (cons (first vals)
           (rule-values (rest rules) (rest vals) matches?))]
    [else
     (rule-values (rest rules) (rest vals) matches?)]))

(define (find-common-ruleset rules tkts)
  ; generate permutations of rule-positions
  (define rps (rule-positions rules tkts))
  (define ps (perm-sublists rps))
  (matching-ruleset rules ps tkts))

(define (matching-ruleset rules ps tkts)
  "find the set of rule positions ps that fulfill all tickets"
  (define (verify-loop p ts)
    (if (empty? p) #t
        (and (match-rule-all? (map first ts)
                              (list-ref rules (first p)))
             (verify-loop (rest p) (map rest ts)))))
  (define (loop p [i 1])
    (displayln (format "verifying ~v of ~v: ~v" i (length p) (first p)))
    (if (verify-loop (first p) tkts)
        (map (lambda (v) (list-ref rules v)) (first p))
        (loop (rest p) (add1 i))))
  (loop ps))

(define (rule-positions rules tkts)
  (if (= (length (first tkts)) 0)
      (list)
      (let ([fvs (map car tkts)])
        (cons
         (filter-map (lambda (r i) (and (and-list fvs (lambda (v) (match-rule? v r))) i))
                     rules
                     (range 0 (length rules)))
         (rule-positions rules (map rest tkts))))))

  ; find all rules that conform to position 1 and then 

(define (part-a)
  (define input (parse-input (file->lines "data/DaySixteen.txt")))
  (apply + (all-invalid-rules input)))

(define (part-b)
  (define input (parse-input (file->lines "data/DaySixteen.txt")))
  (define valids (only-valid-tickets input))
  (define rules (find-common-ruleset (ticket-rules input) valids))
  (apply * (rule-values rules (ticket-mine input) (lambda (s) (string-prefix? s "departure")))))

(module+ test
  (require rackunit)

  (define example-rules
    (map parse-rule
         (list "class: 1-3 or 5-7"
               "row: 6-11 or 33-44"
               "seat: 13-40 or 45-50")))

  (check-equal?
   example-rules
   '(("class" (1 . 3) (5 . 7))
     ("row" (6 . 11) (33 . 44))
     ("seat" (13 . 40) (45 . 50))))

  (check-equal?
   (any-invalid-rule example-rules (list 40 4 50))
   (list 4))

  (define example-input
    (list "class: 1-3 or 5-7"
          "row: 6-11 or 33-44"
          "seat: 13-40 or 45-50"
          ""
          "your ticket:"
          "7,1,14"
          ""
          "nearby tickets:"
          "7,3,47"
          "40,4,50"
          "55,2,20"
          "38,6,12"))

  (check-equal?
   (parse-input example-input)
   (ticket example-rules
           '(7 1 14)
           '((7 3 47)
             (40 4 50)
             (55 2 20)
             (38 6 12))))

  (check-equal?
   (all-invalid-rules (parse-input example-input))
   '(4 55 12))

  (check-equal?
   (only-valid-tickets (parse-input example-input))
   '((7 3 47)))

  (define example-input-b
    (list "class: 0-1 or 4-19"
          "row: 0-5 or 8-19"
          "seat: 0-13 or 16-19"
          ""
          "your ticket:"
          "11,12,13"
          ""
          "nearby tickets:"
          "3,9,18"
          "15,1,5"
          "5,14,9"))

  (define example-tickets-b
    (parse-input example-input-b))

  (check-equal?
   (find-common-ruleset (ticket-rules example-tickets-b) (ticket-others example-tickets-b))
   '(("row" (0 . 5) (8 . 19)) ("class" (0 . 1) (4 . 19)) ("seat" (0 . 13) (16 . 19))))

  (check-equal?
   (rule-values '(("row" (0 . 5) (8 . 19)) ("class" (0 . 1) (4 . 19)) ("seat" (0 . 13) (16 . 19)))
                '(1 2 3)
                (lambda (s) (string-prefix? s "cl")))
   '(2))
)
  
    
