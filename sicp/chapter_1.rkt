#lang racket

; exercise 1.3 define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers
(define (square x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
; basic
(define (biggest-squares a b c)
  (cond
    [(and (>= a c) (>= b c))
     (sum-of-squares a b)]
    [(and (>= b a) (>= c a))
     (sum-of-squares c b)]
    [else
     (sum-of-squares a c)]))
; racket
(define (biggest-squares-rev a b c)
  (apply biggest-squares (sort (list a b c) >)))

(module+ test
  (require rackunit)

  (check-equal?
   (biggest-squares 5 3 2)
   34)

  (check-equal?
   (biggest-squares 5 2 3)
   34)
  
  (check-equal?
   (biggest-squares 2 3 5)
   34)

  (check-equal?
   (biggest-squares 8 -3 -27)
   73)

  (check-equal?
   (biggest-squares-rev 5 3 2)
   34)

  (check-equal?
   (biggest-squares-rev 5 2 3)
   34)
  
  (check-equal?
   (biggest-squares-rev 2 3 5)
   34)

  (check-equal?
   (biggest-squares-rev 8 -3 -27)
   73)
)

; exercise 1.4 what does the following do?
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; adds b if it is positive, negates it if it is negative, making b always be a positive number
; confirmed:
; (a-plus-abs-b 1 -1) => 2
; (a-plus-abs-b 1 1) => 2

; 1.5 testing for applicatie-order evaluation or normal-order evaluation
(define (applicative-tests)
  (define (p) (p))
  (define (test x y)
    (if (= x 0) 0 y))
  (test 0 (p))
  
  ; applicative order will infinitely expand (p) to itself
  (test 0 (p))

  ; normal-order evaluation, expression evaluates step by step
  (test 0 (p)) 
  (if (= 0 0) 0 (p)) 
  (if #t 0 (p)) 
  0)

; 1.22 find three smallest primes for ranges 1000 10000 100000 1000000, how long did each take?
(define (benchmark-smallest-primes)
  (define rngs (list 1000 10000 100000 1000000))
  (define (find-until-3 n [acc (list)])
    (if (= (length acc) 3) acc
        (find-until-3 (next-skip-evens n)
                      (if (prime? n) (cons n acc) acc))))
  (for ([pr rngs])
      (let ([result (time (find-until-3 pr))])
        (displayln (format "Primes: ~v" result)))))

; 1.23 skip further event numbers after 2, how much does this improve performance?
(define (next-skip-evens n)
  (if (odd? n)
      (+ n 2)
      (+ n 1)))

(define (prime? n [next add1])

  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond [(> (square test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor n (next test-divisor))]))
  
  (define (divides? a b) (= (remainder b a) 0))

  (= n (smallest-divisor n)))

; 1.30 sum generates a linear recursion. Make sum  run iteratively.

(define (sum-rec term a next b)
  (if  (> a b)
       0
       (+ (term a)
          (sum-rec term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(module+ test
  (require rackunit)

  (define (cube n)
    (* n n n))

  (define (sum-cubes a b sum)
    (sum cube a add1 b))

  (check-equal?
   (sum-cubes 1 10 sum-rec)
   3025)

  (check-equal?
   (sum-cubes 1 10 sum-iter)
   3025)

  (define (identity x) x)

  (define (sum-integers a b sum)
    (sum identity a add1 b))

  (check-equal?
   (sum-integers 1 10 sum-rec)
   55)

  (check-equal?
   (sum-integers 1 10 sum-iter)
   55)
    
)

; exercise 1.32 show that sum and product are both special cases of a still more general notion called accumulate
; that combines a collection of terms, using some general accumulation function

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(define (sum-acc-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (mult-acc-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (sum-acc-rec term a next b)
  (accumulate-rec + 0 term a next b))

(define (mult-acc-rec term a next b)
  (accumulate-rec * 1 term a next b))

(module+ test
  (require rackunit)

  (check-equal?
   (sum-cubes 1 10 sum-acc-iter)
   3025)

  (check-equal?
   (sum-cubes 1 10 sum-acc-rec)
   3025)

  (check-equal?
   (sum-integers 1 10 sum-acc-iter)
   55)

  (check-equal?
   (sum-integers 1 10 sum-acc-rec)
   55)

  (check-equal?
   (mult-acc-iter identity 1 add1 5)
   (* 1 2 3 4 5))
   
  (check-equal?
   (mult-acc-rec identity 1 add1 5)
   (* 1 2 3 4 5))
    
)

; exercise 1.33 implement accumulate with a filter option

(define (accumulate-filter combiner null-value term a next b fltr?)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (fltr? (term a))
                  (combiner (term a) result)
                  result))))
  (iter a null-value))

(module+ test
  (require rackunit)

  (check-equal?
   (accumulate-filter + 0 identity 1 add1 10 odd?)
   (+ 1 3 5 7 9))
)

; exrecise 1.41 define a procedure double that take a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2.
; What value is returned by (((double (double double)) inc) 5)

(define (double f)
  (lambda (v) (f (f v))))

(module+ test
  (require rackunit)

  (check-equal?
   ((double add1) 1)
   3)

  (check-equal?
   (((double (double double)) add1) 5)
   21)
)

; exercise 1.43 create compose x -> f(g(x))
; for example ((compose square inc) 6) -> 49
(define (compose f g)
  (lambda (v) (f (g v))))

(module+ test
  (require rackunit)

  (check-equal?
   ((compose square add1) 6)
   49)
)

; exercise 1.43 implement a function that can repeatedly apply f n times
; hint, use compose
(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (sub1 n)))))

(module+ test
  (require rackunit)

  (check-equal?
   ((repeated add1 2) 1)
   3)
)

; exercise 1.46 implement iterative improvement
; a procedure that takes procedures as arguments: a method for telling if a guess is good enough, and a method for improving the guess
; returns a procedure that should take a guess as argument and keeps improving guess until it is good enough
(define (iterative-improvement initial improve good-enough?)
  (define (loop current prev)
    (if (good-enough? current prev) current 
        (loop (improve current prev) current)))
  (lambda (guess)
    (loop initial guess)))

; implement sqrt from 1.1.7
(define (iterative-improve good-enough? improve) 
   (lambda (guess) 
     (if (good-enough? guess) 
         guess 
         ((iterative-improve good-enough? improve)
          (improve guess))))) 
  
 (define (sqrt x) 
   ((iterative-improve 
     (lambda (y) 
       (< (abs (- (square y) x)) 
          0.0001)) 
     (lambda (y) 
       (average y (/ x y)))) 
    1.0))

(define (average x y)
  (/ (+ x y) 2))
  
(module+ test
  (require rackunit)

  (check-equal?
   ((iterative-improve (lambda (v) (equal? v 5))
                       add1)
    1)
   5)

)
