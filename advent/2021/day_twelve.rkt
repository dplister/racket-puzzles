#lang racket

(struct explored (dirs) #:transparent)

(struct dir (from to) #:transparent)

(define (parse-caves lines)
  (flatten (map (lambda (line)
         (let ([tokens (string-split line "-")])
           (list (dir (first tokens) (second tokens))
                 (dir (second tokens) (first tokens)))))
                lines)))

(define (uppercase? str)
  (string=? str (string-upcase str)))

(define (lowercase? str)
  (string=? str (string-downcase str)))

(define (single-visit-rule? d path)
  (or (uppercase? (dir-to d))
      (not (in-path? (dir-to d) path))))

(define (just-this-once-rule? d path)
  (and (not (equal? (dir-to d) "start"))
       (or (uppercase? (dir-to d))
           (not (has-double-visited? path))
           (not (in-path? (dir-to d) path)))))
      
(define (has-double-visited? path)
  (has-duplicate? (sort (filter lowercase? (map dir-to path)) string<?)))

(define (has-duplicate? lst [prev #f])
  (cond
    [(empty? lst) #f]
    [(not prev) (has-duplicate? (rest lst) (first lst))]
    [(equal? (first lst) prev) #t]
    [else (has-duplicate? (rest lst) (first lst))]))
      
(define (explore-paths dirs rule [path (list (dir "" "start"))])
  ; find all exits from current path not fully explored (smalls) already
  (define current (first path))
  (if (equal? (dir-to current) "end") (explored path)
      (begin
        (let* ([exits
                (filter (lambda (d)
                          (and (equal? (dir-from d) (dir-to current))
                               (rule d path)))
                        dirs)]
               [possibles 
                (map (lambda (e) (explore-paths dirs rule (cons e path))) exits)])
          (flatten possibles)))))

(define (get-paths dirs rule)
  (define paths (explore-paths dirs rule))
  (map (lambda (p) (string-join (map dir-to (reverse (explored-dirs p))) ",")) paths))

(define (reached-exit? path)
  (and (not (empty? path))
       (equal? (dir-to (first path)) "end")))

(define (in-path? dname path)
  (not (empty? (filter (lambda (p) (equal? dname (dir-to p))) path))))

(define (part-a)
  (define caves (parse-caves (file->lines "data/DayTwelve.txt")))
  (length (get-paths caves single-visit-rule?)))
  
(define (part-b)
  (define caves (parse-caves (file->lines "data/DayTwelve.txt")))
  (length (get-paths caves just-this-once-rule?)))

(module+ test
  (require rackunit)

  (check-equal?
   (parse-caves (list "start-A" "A-C"))
   (list (dir "start" "A") (dir "A" "start")
         (dir "A" "C") (dir "C" "A")))

  (for ([input (list "aSD" "ASD" "asd" "aSd" "ASd")]
        [expected (list #f #t #f #f #f)])
    (check-equal?
     (uppercase? input)
     expected))

  ; part a

  (define small-example-data
    (parse-caves (list "start-A"
                       "start-b"
                       "A-c"
                       "A-b"
                       "b-d"
                       "A-end"
                       "b-end")))
  (define small-example-result
   (sort (list
    "start,A,b,A,c,A,end"
    "start,A,b,A,end"
    "start,A,b,end"
    "start,A,c,A,b,A,end"
    "start,A,c,A,b,end"
    "start,A,c,A,end"
    "start,A,end"
    "start,b,A,c,A,end"
    "start,b,A,end"
    "start,b,end") string<?))
    
  (check-equal?
   (sort (get-paths small-example-data single-visit-rule?) string<?)
   small-example-result)

  (define medium-example-data
    (parse-caves (list "dc-end"
                       "HN-start"
                       "start-kj"
                       "dc-start"
                       "dc-HN"
                       "LN-dc"
                       "HN-end"
                       "kj-sa"
                       "kj-HN"
                       "kj-dc")))
  (define medium-example-result
    (list "start,HN,dc,HN,end"
          "start,HN,dc,HN,kj,HN,end"
          "start,HN,dc,end"
          "start,HN,dc,kj,HN,end"
          "start,HN,end"
          "start,HN,kj,HN,dc,HN,end"
          "start,HN,kj,HN,dc,end"
          "start,HN,kj,HN,end"
          "start,HN,kj,dc,HN,end"
          "start,HN,kj,dc,end"
          "start,dc,HN,end"
          "start,dc,HN,kj,HN,end"
          "start,dc,end"
          "start,dc,kj,HN,end"
          "start,kj,HN,dc,HN,end"
          "start,kj,HN,dc,end"
          "start,kj,HN,end"
          "start,kj,dc,HN,end"
          "start,kj,dc,end"))
  (check-equal?
   (sort (get-paths medium-example-data single-visit-rule?) string<?)
   medium-example-result)

  (define large-example-data
    (parse-caves (list "fs-end"
                       "he-DX"
                       "fs-he"
                       "start-DX"
                       "pj-DX"
                       "end-zg"
                       "zg-sl"
                       "zg-pj"
                       "pj-he"
                       "RW-he"
                       "fs-DX"
                       "pj-RW"
                       "zg-RW"
                       "start-pj"
                       "he-WI"
                       "zg-he"
                       "pj-fs"
                       "start-RW")))
  (check-equal?
   (length (get-paths large-example-data single-visit-rule?))
   226)

  ; part b
  (define small-example-b-result
    (list "start,A,b,A,b,A,c,A,end"
          "start,A,b,A,b,A,end"
          "start,A,b,A,b,end"
          "start,A,b,A,c,A,b,A,end"
          "start,A,b,A,c,A,b,end"
          "start,A,b,A,c,A,c,A,end"
          "start,A,b,A,c,A,end"
          "start,A,b,A,end"
          "start,A,b,d,b,A,c,A,end"
          "start,A,b,d,b,A,end"
          "start,A,b,d,b,end"
          "start,A,b,end"
          "start,A,c,A,b,A,b,A,end"
          "start,A,c,A,b,A,b,end"
          "start,A,c,A,b,A,c,A,end"
          "start,A,c,A,b,A,end"
          "start,A,c,A,b,d,b,A,end"
          "start,A,c,A,b,d,b,end"
          "start,A,c,A,b,end"
          "start,A,c,A,c,A,b,A,end"
          "start,A,c,A,c,A,b,end"
          "start,A,c,A,c,A,end"
          "start,A,c,A,end"
          "start,A,end"
          "start,b,A,b,A,c,A,end"
          "start,b,A,b,A,end"
          "start,b,A,b,end"
          "start,b,A,c,A,b,A,end"
          "start,b,A,c,A,b,end"
          "start,b,A,c,A,c,A,end"
          "start,b,A,c,A,end"
          "start,b,A,end"
          "start,b,d,b,A,c,A,end"
          "start,b,d,b,A,end"
          "start,b,d,b,end"
          "start,b,end"))

  (check-equal?
   (sort (get-paths small-example-data just-this-once-rule?) string<?)
   (sort small-example-b-result string<?))

  (check-equal?
   (length (get-paths medium-example-data just-this-once-rule?))
   103)

  (check-equal?
   (length (get-paths large-example-data just-this-once-rule?))
   3509)
)
