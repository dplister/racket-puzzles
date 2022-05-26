#lang racket

(require "../prelude.rkt")

(define (tokenize input)
  "converts raw inputs into numbers, operators, and sublists"
  (define (loop ls [acc (list)])
    (if (empty? ls) acc
        (loop (rest ls)
              (append acc
                      (match (first ls)
                        [(regexp #rx"(\\()([0-9]+)" (list _ opbra num))
                         (list #\( (string->number num))]
                        [(regexp #rx"([0-9]+)(\\))" (list _ num clobra))
                         (list (string->number num) #\))]
                        [(app string->number (? number? num))
                         (list num)]
                        ["+" (list +)]
                        ["-" (list -)]
                        ["*" (list *)]
                        ["/" (list /)])))))
  (loop (string-split input)))

(define (resolve tokens)
  (define (calculate tokens [acc 0])
    (match tokens
      ['() (values tokens acc)]
      [(list #\) r ___)
       (values r acc)]
      [(list (? number? n) r ___)
       (calculate r (+ acc n))]
      [(list op #\( r ___)
       (let-values ([(remaining subval) (calculate r 0)])
         (calculate remaining (op acc subval)))]
      [(list #\( r ___)
       (let-values ([(remaining subval) (calculate r 0)])
         (calculate remaining subval))]
      [(list op (? number? n) r ___)
       (calculate r (op acc n))]))
  (define (loop tkns acc)
    (displayln (format "tkns ~v acc ~v" tkns acc))
    (let-values ([(tkns acc) (calculate tkns acc)])
      (if (empty? tkns) acc
          (loop tkns acc))))
  (loop tokens 0))

(define (part-a)
  (define inputs (file->lines "data/DayEighteen.txt"))
  (foldl + 0 (map (lambda (line) (resolve (tokenize line))) inputs)))

(module+ test
  (require rackunit)

  (for ([input (list
                "1"
                "(1 + 2)"
                "(1 + 2) * 3 / 2")]
        [expected (list
                   (list 1)
                   (list #\( 1 + 2 #\))
                   (list #\( 1 + 2 #\) * 3 / 2))])
    (check-equal? 
     (tokenize input)
     expected))

    (check-equal?
     (resolve (list 1 + 1))
     2)
    (check-equal?
     (resolve (tokenize "(1 + 1)"))
     2)
    (check-equal?
     (resolve (tokenize "1 + (3 * 2)"))
     7)

    ; example from advent
    (check-equal?
     (resolve (tokenize "1 + 2 * 3 + 4 * 5 + 6"))
     71)
    (check-equal?
     (resolve (tokenize "1 + (2 * 3) + (4 * (5 + 6))"))
     51)
    (check-equal?
     (resolve (tokenize "2 * 3 + (4 * 5)"))
     26)
    (check-equal?
     (resolve (tokenize "((1 + 2) + 3) + 5 + (4 * 5)"))
     31)
    (check-equal?
     (resolve (tokenize "5 + (8 * 3 + 9 + 3 * 4 * 3)"))
     437)
    (check-equal?
     (resolve (tokenize "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))
     12240)
    (check-equal?
     (resolve (tokenize "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))
     13632)
    (check-equal?
     (resolve (tokenize "((7 * 8 + 2) * 8 * 6 * 5)"))
     13920)
    (check-equal?
     (resolve (tokenize "((8 + 3 * 9 + 7) + 8 * 5)"))
     570)
    (check-equal?
     (resolve (tokenize "((7 * 8 + 2) * 8 * 6 * 5) + 4 + ((8 + 3 * 9 + 7) + 8 * 5)"))
     14494)
    (check-equal?
     (resolve (tokenize "((7 * 8 + 2) * 8 * 6 * 5) + 4 + ((8 + 3 * 9 + 7) + 8 * 5) + (2 + 5 + 8) * 4"))
     58036)
)

