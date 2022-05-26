#lang racket

(define (find-total mp val)
  "returns the set of numbers in map mp that equal up to val or #f if not present"
  (define (loop ls)
    (if (empty? ls) #f
        (let ([tf (- val (first ls))])
          (if (and (not (equal? tf (first ls)))
                   (dict-has-key? mp tf))
              (list (first ls) tf)
              (loop (rest ls))))))
  (loop (dict-keys mp)))

(define (first-invalid numbers preamble-limit)
  (define (loop pre curr rem mp)
    ;(displayln "--------------")
    ;(displayln (format "pre ~v curr ~v rem ~v" pre curr rem))
    ;(displayln (format "mp ~v" mp))
    (cond
      [(< (length pre) preamble-limit)
       (loop (cons curr pre) (first rem) (rest rem) (dict-set mp curr 1))]
      [(not (find-total mp curr)) curr]
      [(empty? rem) #f]
      [else
       (loop (cons curr (take pre (sub1 preamble-limit)))
             (first rem)
             (rest rem)
             (dict-set (dict-remove mp (last pre)) curr 1))]))
  (loop (list) (first numbers) (rest numbers) #hash()))

(define (consecutive-values ls target [acc (list)])
  "finds set of consecutive values in ls that equal target"
  (define racc (apply + acc))
  (cond
    [(> racc target) (consecutive-values ls target (drop-right acc 1))]
    [(< racc target) (consecutive-values (rest ls) target (cons (first ls) acc))]
    [else (reverse acc)]))

(define (part-a)
  (define ls (map string->number (file->lines "data/DayNine.txt")))
  (first-invalid ls 25))

(define (part-b)
  (define ls (map string->number (file->lines "data/DayNine.txt")))
  (define result (sort (consecutive-values ls (first-invalid ls 25)) <))
  (+ (first result) (last result)))

(module+ test
  (require rackunit)

  (check-equal?
   (find-total #hash((1 . 1) (2 . 1) (3 . 1)) 3)
   (list 1 2))

  (define (hash-vals vals)
    (make-hash (map (lambda (v) (cons v 1)) vals)))

  (check-equal?
   (find-total (hash-vals (list 35 20 15 25 47)) 40)
   (list 25 15))

  (check-equal?
   (first-invalid (range 1 27) 25) ; 26 is valid because 1, 25
   #f)

  (check-equal?
   (first-invalid (range (- 49 25) 50) 25) ; 49 is valid because 24, 25
   #f)

  (check-equal?
   (first-invalid (range (- 100 25) 101) 25) ; 100 not valid
   100)

  (define example-data
    (list 35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576))

  (check-equal?
   (first-invalid example-data 5)
   127)

  (check-equal?
   (consecutive-values example-data 127)
   (list 15 25 47 40))
  
)
