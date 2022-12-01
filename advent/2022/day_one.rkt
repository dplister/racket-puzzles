#lang racket

(require "../prelude.rkt")

(define (total ls)
  "totals the set of numbers in each list"
  (map (lambda (l) (apply + l)) ls))

(define (number-or-nan input)
  "converts input to number or NaN"
  (match input
    [(app string->number (? number? n)) n]
    [else 'NaN]))

(define (process-input lines)
  "converts lines into a set of numbers split by gaps"
  (define numbered (map number-or-nan lines))
  (sublists numbered (lambda (v) (equal? v 'NaN)) #:include-break #f))

(define (part-a lines)
  (define numbers (process-input lines))
  (define totals (total numbers))
  (apply max totals))

(define (part-b lines)
  (define numbers (process-input lines))
  (define totals (total numbers))
  (apply + (take (sort totals >) 3)))

(part-a (file->lines "day_one.txt"))
(part-b (file->lines "day_one.txt"))

(module+ test
  (require rackunit)

  (define example
    (list "1000" "2000" "3000" "" "4000" "" "5000" "6000" "" "7000" "8000" "9000" "" "10000"))

  (check-equal?
    (process-input example)
    '((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000)))

  (check-equal?
    (part-a example)
    24000)

  (check-equal?
    (part-b example)
    45000)

)
