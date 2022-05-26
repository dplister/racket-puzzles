#lang racket

(struct clock (digits face))

(struct number-map ([number #:mutable] digits) #:transparent)

(define (parse-clock line)
  (define parts (string-split line "|"))
  (clock
   (string-split (first parts))
   (string-split (second parts))))

(define (face-number input)
  (cond
    [(= (string-length input) 2) 1]
    [(= (string-length input) 3) 7]
    [(= (string-length input) 4) 4]
    [(= (string-length input) 7) 8]
    [else 'unknown]))

(define (identify-digits c)
  (filter (lambda (n) (number? n))
          (map face-number (clock-face c))))

(define (count-digits lines)
  (define clocks (map parse-clock lines))
  (length (flatten (map identify-digits clocks))))

(define (map-digits digit-sets)
  (map (lambda (ds)
         (number-map
          (face-number ds)
          (string->list ds)))
       digit-sets))

(define (left-over from to)
  (filter (lambda (l) (not (member l from))) to))

(define (union from to)
  (filter (lambda (l) (member l from)) to))

(define (get-letters m digit)
  (number-map-digits
   (first
    (filter (lambda (v)
              (and
               (number? (number-map-number v))
               (equal? (number-map-number v) digit)))
            m))))

(define (is-unknown nm)
  (filter (lambda (n) (not (number? (number-map-number n)))) nm))

(define (has-letter nm letter)
  (filter (lambda (n) (member letter (number-map-digits n))) nm))

(define (get-letters-by-length m len)
  (filter (lambda (v)
            (= (length (number-map-digits v)) len))
          m))

(define (translate-clock c)
  (define trans (map-letters (clock-digits c)))
  (map (translate-digit trans)
       (map string->list (clock-face c))))

(define (translate-digit mp)
  (lambda (ds)
    (number-map-number
     (first (filter (lambda (p)
                      (has-all-digits (number-map-digits p) ds)) mp)))))

(define (has-all-digits xs ys)
  (= (length xs) (length ys) (length (union xs ys))))

(define (line-to-clock-digits line)
  (define c (parse-clock line))
  (translate-clock c))
   
(define (map-letters letter-sets)
  (define nm (map-digits letter-sets))
  (define lm (make-hash))
  ; map top (a)
  (define one-digits (get-letters nm 1))
  (dict-set! lm #\a (first (left-over (get-letters nm 1) (get-letters nm 7))))
  ; work out six, then we know location of bottom right (f) and top right (c)
  (define six-lens (get-letters-by-length nm 6))
  (define six
    (first (filter (lambda (l)
                     (= 1 (length (left-over (number-map-digits l) (get-letters nm 1)))))
            six-lens)))
  (set-number-map-number! six 6)
  ; set top right (c)
  (dict-set! lm #\c (first (left-over (number-map-digits six) (get-letters nm 1))))
  ; set bottom right (f)
  (dict-set! lm #\f (first (left-over (list (dict-ref lm #\c)) (get-letters nm 1))))
  lm
  ; 3 has 5 filled in and is the only one that meets the requirements for 1
  (define five-lens (get-letters-by-length nm 5))
  (define three
    (first (filter (lambda (l)
                     (= 0 (length (left-over (number-map-digits l) (get-letters nm 1)))))
                   five-lens)))
  (set-number-map-number! three 3)
  ; 3 and 4 have c d f in common, with c and f already known
  (dict-set! lm #\d (first
              (left-over
               (get-letters nm 1)
               (union (get-letters nm 3) (get-letters nm 4)))))
  ; 0, 9, 6 all have 6, but we know pos d and 6 so we can work out 9
  (define nine (first (has-letter (is-unknown (get-letters-by-length nm 6))
                                  (dict-ref lm #\d))))
  (set-number-map-number! nine 9)
  (define zero (first (is-unknown (get-letters-by-length nm 6))))
  (set-number-map-number! zero 0)
  ; 2 has c
  (define two (first (has-letter (is-unknown (get-letters-by-length nm 5))
                                 (dict-ref lm #\c))))
  (set-number-map-number! two 2)
  ; 5 has f
  (define five (first (has-letter (is-unknown (get-letters-by-length nm 5))
                                  (dict-ref lm #\f))))
  (set-number-map-number! five 5)
  nm
  )

;; TESTING
; (map-letters (list "acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"))

(define (part-a)
  (define lines (file->lines "data/DayEight.txt"))
  (count-digits lines))

(define (part-b)
  (define lines (file->lines "data/DayEight.txt"))
  (foldl + 0 (map undigits (map line-to-clock-digits lines))))

; from prog prax - prelude
(define (undigits ds . args)
  (let ((b (if (null? args) 10 (car args))))
    (let loop ((ds ds) (n 0))
      (if (null? ds) n
          (loop (cdr ds) (+ (* n b) (car ds)))))))

(module+ test
  (require rackunit)

  (check-equal?
   (map face-number (list "fdgacbe" "cefdb" "cefbgd" "gcbe"))
   (list 8 'unknown 'unknown 4))

  (check-equal?
   (identify-digits (clock (list) (list "fdgacbe" "cefdb" "cefbgd" "gcbe")))
   (list 8 4))

  (define example-one
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf")

  (check-equal?
   (line-to-clock-digits example-one)
   (list 5 3 5 3))
  
  (define example-input
    (list
     "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
     "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
     "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
     "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
     "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
     "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
     "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
     "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
     "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
     "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

  (check-equal?
   (count-digits example-input)
   26)

  (check-equal?
   (map line-to-clock-digits example-input)
   (list
    (list 8 3 9 4)
    (list 9 7 8 1)
    (list 1 1 9 7)
    (list 9 3 6 1)
    (list 4 8 7 3)
    (list 8 4 1 8)
    (list 4 5 4 8)
    (list 1 6 2 5)
    (list 8 7 1 7)
    (list 4 3 1 5)))
  )
