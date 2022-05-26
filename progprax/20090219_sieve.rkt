#lang racket

(define (sieve-n ls n)
  (filter (lambda (v) (not (= (modulo v n) 0))) ls))

(define (sieve mx)
  (define smx (sqrt mx))
  (define (loop ls)
    (if (> (first ls) smx)
        ls
        (cons (first ls)
              (loop (sieve-n (rest ls)
                             (first ls))))))
  (loop (range 2 (+ mx 1))))
  
(length (sieve 15485863)) ; 1000000

(module+ test
  (require rackunit)

  (check-equal? (sieve-n (range 3 31) 2)
                (list 3 5 7 9 11 13 15 17 19 21 23 25 27 29))
  (check-equal? (sieve-n (list 5 7 9 11 13 15 17 19 21 23 25 27 29) 3)
                (list 5 7 11 13 17 19 23 25 29))

  (check-equal? (sieve 30)
                (list 2 3 5 7 11 13 17 19 23 29))
)
