#lang racket

(provide (all-defined-out))

(struct result (value))

(define (remove-list xs rs)
  (foldl (lambda (x r) (remove x r)) xs rs))

(define (difference l1 l2)
  (define lm (remove-list l1 l2))
  (define rm (remove-list l2 l1))
  (append lm rm))

(define (intersect fl . rst)
  "takes multiple lists and finds the intersection amongst all of them"
  (define (loop ref lst)
    (if (empty? lst) ref
        (loop (remove-list ref (difference ref (first lst))) (rest lst))))
  (loop fl rst))

(define (vector-last vec)
  (define vl (vector-length vec))
  (if (= vl 0) #f (vector-ref vec (- vl 1))))

(define (binary->number bin)
  (foldl + 0
         (for/list ([i (range 0 (length bin))]
                    [b (reverse bin)])
           (* b (expt 2 i)))))

(define (number->binary num [acc (list)])
  (if (= num 0) acc
      (let ([q (quotient num 2)]
            [r (modulo num 2)])
        (number->binary q (cons r acc)))))

(define (char->num c)
  (match c
    [#\0 0]
    [#\1 1]
    [#\2 2]
    [#\3 3]
    [#\4 4]
    [#\5 5]
    [#\6 6]
    [#\7 7]
    [#\8 8]
    [#\9 9]
    [else #f]))

(define (sublists lines break? #:include-break [include-break #t])
  "splits a list into sublists where break? is true; include-break includes the splitting element as first value"
  (define (loop ls acc)
    (cond
      [(empty? ls) (cons (reverse acc) (list))]
      [(and (break? (first ls))
            (not (empty? acc)))
       (cons (reverse acc)
             (loop (rest ls)
                   (if include-break (list (first ls)) (list))))]
      [else
       (loop (rest ls) (cons (first ls) acc))]))
  (loop lines (list)))

(define (equal-f target)
  "returns a lambda that checks if the parameter supplied matches target"
  (lambda (v) (equal? target v)))

(define (perms-dupes lst [acc (list)])
  (if (= (length lst) (length acc)) (list acc)
      (append-map (lambda (l) (perms-dupes lst (cons l acc))) lst)))

(define (perm-sublists lst #:dupes [dupes? #f])
  "generates the set of permutations at each list position"
  (define (loop ls acc)
    (if (empty? ls) (result (reverse acc))
        (let ([res (filter (lambda (v) (or dupes? (not (member v acc)))) (first ls))])
          (if (empty? res) #f
              (filter-map (lambda (v) (loop (rest ls) (cons v acc))) res)))))
  (map result-value (flatten (filter-map (lambda (v) (loop (rest lst) (list v))) (first lst)))))

(define (write-file filename output)
  (define outfile (open-output-file filename #:exists 'replace))
  (display output outfile)
  (close-output-port outfile))

(define (map-until f lst end?)
  (if (empty? lst) (list)
      (let ([r (f (first lst))])
        (cons r (if (end? r) (rest lst) (map-until f (rest lst) end?))))))

(define (count-distinct lst [acc #hash()])
  (if (empty? lst) acc
      (count-distinct (rest lst) (dict-update acc (first lst) add1 0))))

(define (map-with-rest l f)
  (if (empty? l) (list)
      (cons (f (first l) (rest l))
            (map-with-rest (rest l) f))))

(define (apply-zip ls fs)
  "zips one f to one l until either list runs out"
  (if (or (empty? ls) (empty? fs))
      ls
      (cons ((first fs) (first ls))
            (apply-zip (rest ls) (rest fs)))))

(define (and-list l f)
  "returns result of f if f is valid for each element of l, otherwise returns #f"
  (define (loop ls [acc (list)])
    (if (empty? ls) (reverse acc)
      (let ([r (f (first ls))])
        (if (not r) #f
            (loop (rest ls) (cons r acc))))))
  (loop l))

(define (or-list f . lsts)
  "ors the f of each car of lsts until an instance returns true"
  (define (loop ls)
    (cond
      [(or (empty? ls) (empty? (first ls))) #f]
      [(apply f (map first ls)) (map first ls)]
      [else (loop (map rest ls))]))
  (loop lsts))

(define (wrap-number n len)
  "ensure the number is between 0 (inclusive) and len (exclusive) by wrapping the number back around to 0"
  (- n (* (quotient n len) len)))

(define (string-split-suffix input ln)
  (values (substring input 0 (- (string-length input) ln))
          (substring input (- (string-length input) ln))))

(define (string-empty? s)
  (= (string-length (string-trim s)) 0))

(define (distance n1 n2)
  "returns distance between any two real numbers"
  (abs (- n1 n2)))

(define (repeat-list lst f n)
  "applies f to lst n times, taking the result of each f and passing it to the next"
  (if (<= n 0) lst
      (repeat-list (f lst) f (sub1 n))))

(define (repeat-list-until-trans lst f continue?)
  "applies f to lst until continue? is #f; continue? takes the result of the current (f lst) and the prev (f lst)"
  (define (loop prev curr)
    (if (not (continue? prev curr))
        curr
        (loop curr (f curr))))
  (loop lst (f lst)))

(define (find-f ls accessor comparator)
  (foldl (lambda (c res) (if (comparator (accessor c) res)
                             (accessor c)
                             res))
         (accessor (first ls)) (rest ls)))

(define (min-f ls accessor)
  (find-f ls accessor <))

(define (max-f ls accessor)
  (find-f ls accessor >))

(module+ test
  (require rackunit)

  (check-equal?
   (vector-last (vector 123 456))
   456)

  (check-equal?
   (vector-last (make-vector 0))
   #f)

  (check-equal?
   (binary->number (list 0 1 1 1 1 1 1 0 0 1 0 1))
   2021)

  (check-equal?
   (sublists (list "fn" 1 2 3 "sn" 4 5 6) (lambda (n) (not (number? n))))
   (list (list "fn" 1 2 3) (list "sn" 4 5 6)))
  (check-equal?
   (sublists (list 1 2 3 "split" 4 5 6) (lambda (n) (not (number? n))) #:include-break #f)
   (list (list 1 2 3) (list 4 5 6)))

  (check-equal?
   (map-until add1 (list 1 3 5 6 7 9) odd?)
   (list 2 4 6 7 7 9))

  (check-equal?
   (map-until add1 (list 1 3 5 7 9) odd?)
   (list 2 4 6 8 10))

  (let ([d (count-distinct (list 1 2 3 1 3 2 1 1 3 3))])
    (for ([k (list 1 2 3)]
          [t (list 4 2 4)])
      (check-equal? (dict-ref d k) t)))

  (check-equal?
   (map-with-rest (list 1 2 3)
                  (lambda (v r) (+ v (apply + r))))
   (list 6 5 3))

  (check-equal?
   (apply-zip (list 1 1 1) (list add1 sub1 add1))
   (list 2 0 2))

  (check-equal?
   (apply-zip (list 1 1 1) (list add1 sub1))
   (list 2 0 1))

  (check-equal?
   (and-list (list 1 2 3) odd?)
   #f)

  (check-equal?
   (and-list (list 1 3 5) odd?)
   (list #t #t #t))

  (check-equal?
   (and-list (list 1 3 5) (lambda (v) (and (odd? v) (add1 v))))
   (list 2 4 6))

  (for ([inp (list 5 10 15)]
        [res (list 5 0 5)])
    (check-equal?
     (wrap-number inp 10)
     res))

  (for ([input '((-5 10) (10 5) (10 -5) (5 10))]
        [expected '(15 5 15 5)])
    (check-equal?
     (distance (first input) (second input))
     expected))

  (check-equal?
   (intersect (list 1 2) (list 2 3 4) (list 2 4 5))
   (list 2))
  (check-equal?
   (intersect (list 1 2) (list 3 4) (list 5 6))
   (list))
  (check-equal?
   (intersect (list 1) (list 1) (list 1))
   (list 1))

  ; repeat-list
  (check-equal?
   (repeat-list (list 1 2 3) (lambda (l) (map add1 l)) 1)
   (list 2 3 4))
  (check-equal?
   (repeat-list (list 1 2 3) (lambda (l) (map add1 l)) 2)
   (list 3 4 5))

  ; repeat-list-until-trans
  (check-equal?
   (repeat-list-until-trans (list 1 2 3)
                            (lambda (l) (map add1 l))
                            (lambda (f s) (< (first s) 4)))
   (list 4 5 6))

  ; perm-sublists
  (check-equal?
   (perm-sublists '((1 2) (3 4)))
   '((1 3) (1 4) (2 3) (2 4)))

  (check-equal?
   (perm-sublists '((1 2 3) (4 5 6)))
   '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))

  (check-equal?
   (perm-sublists '((1 2 3) (4 5 6) (7 8 9)))
   '((1 4 7) (1 4 8) (1 4 9) (1 5 7) (1 5 8) (1 5 9) (1 6 7) (1 6 8) (1 6 9)
     (2 4 7) (2 4 8) (2 4 9) (2 5 7) (2 5 8) (2 5 9) (2 6 7) (2 6 8) (2 6 9)
     (3 4 7) (3 4 8) (3 4 9) (3 5 7) (3 5 8) (3 5 9) (3 6 7) (3 6 8) (3 6 9)))

  (check-equal?
   (perm-sublists '((1 2 3) (1 4 5)))
   '((1 4) (1 5) (2 1) (2 4) (2 5) (3 1) (3 4) (3 5)))

  (check-equal?
   (perm-sublists '((1 2) (1 3)) #:dupes #t)
   '((1 1) (1 3) (2 1) (2 3)))

  (check-equal?
   (min-f (list (result 2) (result 1) (result 3)) result-value)
   1)

  (check-equal?
   (max-f (list (result 2) (result 1) (result 3)) result-value)
   3)

  (check-equal?
   (or-list < (list 1 1 1) (list 1 1 1))
   #f)
  (check-equal?
   (or-list < (list 1 1 1) (list 1 2 1))
   '(1 2))
)
   
