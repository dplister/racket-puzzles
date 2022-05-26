#lang racket

(require "../prelude.rkt")

(define (unique-questions lines)
  (remove-duplicates (string->list (apply string-append lines))))

(define (matching-questions lines)
  (apply intersect (map string->list lines)))

(define (party-questions lines question-constraint)
  (map question-constraint
       (map (lambda (ls) (filter (lambda (l) (> (string-length l) 0)) ls)) ; remove blank lines
            (sublists lines (equal-f "")))))

(define (total-questions-answered question-sets)
  (foldl (lambda (current result) (+ (length current) result))
         0 question-sets))

(define (part-a)
  (total-questions-answered (party-questions (file->lines "data/DaySix.txt") unique-questions)))

(define (part-b)
  (total-questions-answered (party-questions (file->lines "data/DaySix.txt") matching-questions)))

(module+ test
  (require rackunit)

  (define one-party-example
    (list "abcx"
          "abcy"
          "abcz"))

  (check-equal?
   (unique-questions one-party-example)
   (list #\a #\b #\c #\x #\y #\z))

  (define multiple-parties-example
    (list "abc"
          ""
         "a"
         "b"
         "c"
         ""
         "ab"
         "ac"
         ""
         "a"
         "a"
         "a"
         "a"
         ""
         "b"))
  
  (check-equal?
   (party-questions multiple-parties-example unique-questions)
   (list (string->list "abc")
         (string->list "abc")
         (string->list "abc")
         (string->list "a")
         (string->list "b")))

  (check-equal?
   (total-questions-answered (party-questions multiple-parties-example unique-questions))
   11)

  (check-equal?
   (matching-questions (take multiple-parties-example 1))
   (string->list "abc"))

  (check-equal?
   (matching-questions (take (drop multiple-parties-example 2) 3))
   (list))

  (check-equal?
   (matching-questions (take (drop multiple-parties-example 6) 2))
   (string->list "a"))
   
  (check-equal?
   (total-questions-answered (party-questions multiple-parties-example matching-questions))
   6)
)
