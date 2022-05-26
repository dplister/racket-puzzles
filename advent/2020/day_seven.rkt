#lang racket

(define (parse-rule input)
  (map (lambda (l)
         (cons (if (> (string-length (first l)) 0) (string->number (first l)) 0)
               (string-trim (second l))))
       (regexp-match* #px"([0-9]*)([a-z ]+?) bag" input #:match-select rest)))

(define (parse-rules lines)
  (map parse-rule lines))

(define (holds bags target)
  "finds the set of bags that can hold target"
  (filter (lambda (bs) (member target (map cdr (rest bs)))) bags))

(define (holds-all bags target)
  (define vals (map bag-name (holds bags target)))
  (if (empty? vals) (list)
      (remove-duplicates (append vals (append-map (lambda (b) (holds-all bags b)) vals)))))

(define (children bags target)
  "lists all the bags that target has"
  (define mbag (filter (lambda (b) (equal? (bag-name b) target)) bags))
  (if (empty? mbag) (list)
      (let ([details (first mbag)])
        (append (rest details)
                (append-map (lambda (b) (multiply-bag-set (children bags (cdr b)) (car b)))
                            (rest details))))))

(define (multiply-bag-set bags multiplier)
  "multiplies all the bag amounts in bags by multiplier"
  (map (lambda (b) (cons (* (car b) multiplier) (cdr b))) bags))

(define (bag-name bag)
  (cdr (first bag)))

(define (part-a)
  (define bags (parse-rules (file->lines "data/DaySeven.txt")))
  (length (holds-all bags "shiny gold")))

(define (part-b)
  (define bags (parse-rules (file->lines "data/DaySeven.txt")))
   (foldl (lambda (l r) (+ (car l) r)) 0
          (children bags "shiny gold")))

(module+ test
  (require rackunit)

  (define basic-rule
    "light gold bags contain 2 light lime bags, 1 faded green bag, 3 clear olive bags, 2 dim bronze bags.")

  (check-equal?
   (parse-rule basic-rule)
   '((0 . "light gold")
     (2 . "light lime")
     (1 . "faded green")
     (3 . "clear olive")
     (2 . "dim bronze")))

  (check-equal?
   (holds (list (parse-rule basic-rule)) "light lime")
   (list (parse-rule basic-rule)))
  (check-equal?
   (holds (list (parse-rule basic-rule)) "light gold")
   (list))

  (define example-data
    (list "light red bags contain 1 bright white bag, 2 muted yellow bags."
          "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
          "bright white bags contain 1 shiny gold bag."
          "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
          "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
          "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
          "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
          "faded blue bags contain no other bags."
          "dotted black bags contain no other bags."))

  (check-equal?
   (map bag-name (holds (parse-rules example-data) "shiny gold"))
   (list "bright white" "muted yellow"))

  (check-equal?
   (holds-all (parse-rules example-data) "shiny gold")
   (list "bright white" "muted yellow" "light red" "dark orange"))

  (check-equal?
   (foldl (lambda (l r) (+ (car l) r)) 0
          (children (parse-rules example-data) "shiny gold"))
   32)

  (define example-data-more
    (list "shiny gold bags contain 2 dark red bags."
          "dark red bags contain 2 dark orange bags."
          "dark orange bags contain 2 dark yellow bags."
          "dark yellow bags contain 2 dark green bags."
          "dark green bags contain 2 dark blue bags."
          "dark blue bags contain 2 dark violet bags."
          "dark violet bags contain no other bags."))

  (check-equal?
   (foldl (lambda (l r) (+ (car l) r)) 0
          (children (parse-rules example-data-more) "shiny gold"))
   126)
   
)
