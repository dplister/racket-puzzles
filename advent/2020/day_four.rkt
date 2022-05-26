#lang racket

(require "../prelude.rkt")

(define (parse-passports lines)
  (map (lambda (l) (append-map parse-passport l))
       (sublists lines (equal-f ""))))

(define (parse-passport line)
  (map (lambda (t)
         (string-split t ":"))
       (string-split line " ")))

(define (except-cid pass)
  (filter (lambda (p) (not (equal? (first p) "cid"))) pass))

(define (is-valid? pass)
  (equal? (length (except-cid pass)) 7))

(define (valid-passports passes)
  (filter is-valid? passes))

(define (validate-passports passes)
  (filter (lambda (p)
            (let ([pcid (except-cid p)])
              (and (equal? (length pcid) 7)
                   (and-list pcid is-valid-field?))))
          passes))

(define (in-number-range? n start end)
  "determines if n is between start and end (inclusive)"
  (and (>= n start) (<= n end)))

(define (is-valid-field? kvp)
  (match (first kvp)
    ["byr" (in-number-range? (string->number (second kvp)) 1920 2002)]
    ["iyr" (in-number-range? (string->number (second kvp)) 2010 2020)]
    ["eyr" (in-number-range? (string->number (second kvp)) 2020 2030)]
    ["hgt"
     #:when (> (string-length (second kvp)) 3)
     (let-values ([(len measure) (string-split-suffix (second kvp) 2)])
       (match measure
         ["cm" (in-number-range? (string->number len) 150 193)]
         ["in" (in-number-range? (string->number len) 59 76)]
         [else #f]))]
    ["ecl" (and (member (second kvp) (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth")) #t)]
    ["pid" (and (string->number (second kvp)) (equal? (string-length (second kvp)) 9))]
    ["cid" #t]
    ["hcl" (and (regexp-match #px"^#[0-9a-f]{6}$" (second kvp)) #t)]
    [else #f]))

(define (part-a)
  (define passes (parse-passports (file->lines "data/DayFour.txt")))
  (length (valid-passports passes)))

(define (part-b)
  (define passes (parse-passports (file->lines "data/DayFour.txt")))
  (length (validate-passports passes)))

(module+ test
  (require rackunit)

  (define example-data
    (list "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
          "byr:1937 iyr:2017 cid:147 hgt:183cm"
          ""
          "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
          "hcl:#cfa07d byr:1929"
          ""
          "hcl:#ae17e1 iyr:2013"
          "eyr:2024"
          "ecl:brn pid:760753108 byr:1931"
          "hgt:179cm"
          ""
          "hcl:#cfa07d eyr:2025 pid:166559648"
          "iyr:2011 ecl:brn hgt:59in"))

  (check-equal?
   (length (valid-passports (parse-passports example-data)))
   2)

  ; individual field tests for part b
  (check-equal? (is-valid-field? (list "byr" "2002")) #t)
  (check-equal? (is-valid-field? (list "byr" "2003")) #f)

  (check-equal? (is-valid-field? (list "hgt" "60in")) #t)
  (check-equal? (is-valid-field? (list "hgt" "190cm")) #t)
  (check-equal? (is-valid-field? (list "hgt" "190in")) #f)
  (check-equal? (is-valid-field? (list "hgt" "190")) #f)
  
  (check-equal? (is-valid-field? (list "hcl" "#123abc")) #t)
  (check-equal? (is-valid-field? (list "hcl" "#123abz")) #f)
  (check-equal? (is-valid-field? (list "hcl" "123abc")) #f)
  
  (check-equal? (is-valid-field? (list "ecl" "brn")) #t)
  (check-equal? (is-valid-field? (list "ecl" "wat")) #f)
  
  (check-equal? (is-valid-field? (list "pid" "000000001")) #t)
  (check-equal? (is-valid-field? (list "pid" "0123456789")) #f)

  (define example-invalid-field-data
    (list "eyr:1972 cid:100"
          "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
          ""
          "iyr:2019"
          "hcl:#602927 eyr:1967 hgt:170cm"
          "ecl:grn pid:012533040 byr:1946"
          ""
          "hcl:dab227 iyr:2012"
          "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
          ""
          "hgt:59cm ecl:zzz"
          "eyr:2038 hcl:74454a iyr:2023"
          "pid:3556412378 byr:2007"))

  (check-equal?
   (length (validate-passports (parse-passports example-invalid-field-data)))
   0)

  (define example-valid-field-data
    (list "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
          "hcl:#623a2f"
          ""
          "eyr:2029 ecl:blu cid:129 byr:1989"
          "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
          ""
          "hcl:#888785"
          "hgt:164cm byr:2001 iyr:2015 cid:88"
          "pid:545766238 ecl:hzl"
          "eyr:2022"
          ""
          "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))

  (check-equal?
   (length (validate-passports (parse-passports example-valid-field-data)))
   4)
  )
