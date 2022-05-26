#lang racket

(require "../prelude.rkt")

(define (parse-rule input)
  (apply-zip
   (rest (regexp-match #rx"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" input))
   (list string->number string->number (compose1 first string->list) string->list)))

(define (valid-count-rule? rule)
  (define rc (third rule))
  (define chars
    (filter (lambda (c) (equal? c rc))
            (fourth rule)))
  (and (>= (length chars) (first rule))
       (<= (length chars) (second rule))))

(define (valid-pos-rule? rule)
  (define rc (third rule))
  (define chars
    (filter (lambda (c) (equal? c rc))
            (list (list-ref (fourth rule) (sub1 (first rule)))
                  (list-ref (fourth rule) (sub1 (second rule))))))
  (= (length chars) 1))

(define (part-a)
  (define rules (map parse-rule (file->lines "data/DayTwo.txt")))
  (length (filter-map valid-count-rule? rules)))

(define (part-b)
  (define rules (map parse-rule (file->lines "data/DayTwo.txt")))
  (length (filter-map valid-pos-rule? rules)))

(module+ test
  (require rackunit)

  (define example-tests
    (list
     (list "1-3 a: abcde" (list 1 3 #\a (list #\a #\b #\c #\d #\e)))
     (list "1-3 b: cdefg" (list 1 3 #\b (list #\c #\d #\e #\f #\g)))
     (list "2-9 c: ccccccccc" (list 2 9 #\c (list #\c #\c #\c #\c #\c #\c #\c #\c #\c)))))

  (for ([t example-tests])
    (check-equal?
     (parse-rule (first t))
     (second t)))

  (for ([t example-tests]
        [r (list #t #f #t)])
    (check-equal?
     (valid-count-rule? (second t))
     r))

  (for ([t example-tests]
        [r (list #t #f #f)])
    (check-equal?
     (valid-pos-rule? (second t))
     r))

  )
