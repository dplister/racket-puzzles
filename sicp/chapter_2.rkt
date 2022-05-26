#lang racket

; exercise 2.2 implement line segments in plane
(define (make-segment p1 p2)
  (cons p1 (cons p2 `())))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cadr l))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(module+ test
  (require rackunit)

  (define example
    (make-segment
     (make-point 1 2)
     (make-point 3 4)))

  (check-equal?
   (start-segment example)
   (make-point 1 2))

  (check-equal?
   (end-segment example)
   (make-point 3 4))

  (check-equal?
   (x-point (start-segment example))
   1)

  (check-equal?
   (y-point (start-segment example))
   2)
)

; 2.4 Here is an alternative procedural representation of pairs.
; Verify (car (cons x y)) yields x for any objects x and y
(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

; define cdr
(define (my-cdr z)
  (z (lambda (p q) q)))

(module+ test
  (require rackunit)

  (check-equal?
   (my-car (my-cons 1 2))
   1)
  (check-equal?
   (my-car (my-cons 2 1))
   2)

  (check-equal?
   (my-cdr (my-cons 1 2))
   2)
  (check-equal?
   (my-cdr (my-cons 2 1))
   1)
)

; 2.6 Representing numbers as procedures
(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add a b) 
  (lambda (f) 
    (lambda (x) 
      ((a f) ((b f) x)))))

(module+ test
  (require rackunit)

  (check-equal?
   (((add two zero) identity) 2)
   2)
)

; Exercise 2.17 Implement last-pair
(define (last-pair ls)
  (if (= (length ls) 1) ls
      (last-pair (cdr ls))))

(module+ test
  (require rackunit)

  (check-equal?
   (last-pair (list 1 2 3))
   (list 3)))

; Exercise 2.18 implement reverse
(define (my-reverse ls)
  (define (loop l acc)
    (if (null? l) acc
        (loop (cdr l) (cons (car l) acc))))
  (loop ls (list)))

(module+ test
  (require rackunit)

  (check-equal?
   (my-reverse (list 1 2 3))
   (list 3 2 1)))

; Exercise 2.19 Change counting with lists
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(define except-first-denomination cdr)
(define first-denomination car)
(define no-more? null?)

(module+ test
  (require rackunit)

  (check-equal?
   (cc 100 us-coins)
   292))

; Exercise 2.20 implement a dotted-tail notation form function that returns a list of all arguments that have the same even-odd parity as the first argument
(define (same-parity i . ls)
  (define m (modulo i 2))
  (define (loop l)
    (if (null? l) (list)
      (if (= (modulo (car l) 2) m)
          (cons (car l) (loop (cdr l)))
          (loop (cdr l)))))
  (cons i (loop ls)))

(module+ test
  (require rackunit)

  (check-equal?
   (same-parity 1 2 3 4 5 6 7)
   '(1 3 5 7))

  (check-equal?
   (same-parity 2 3 4 5 6 7)
   '(2 4 6)))

; exercise 2.27 implement deep reverse
(define (deep-reverse lst)
  (define (loop node acc)
    (cond
      [(null? node) acc]
      [(pair? node)
       (loop (rest node)
             (cons (loop (car node) '()) acc))]
      [else node]))
  (loop lst '()))

(module+ test
  (require rackunit)

  (check-equal?
   (deep-reverse '((1 2) (3 4)))
   '((4 3) (2 1)))

  (check-equal?
   (deep-reverse '((1 2 3) (4 5 6)))
   '((6 5 4) (3 2 1)))

)

; exercise 2.28 implement fringe

(define (fringe lst)
  (define (loop n acc)
    (cond
      [(null? n) acc]
      [(pair? n)
       (loop (cdr n) (loop (car n) acc))]
      [else (cons n acc)]))
  (reverse (loop lst '())))

(module+ test
  (require rackunit)

  (define x '((1 2) (3 4)))
  (check-equal? (fringe x)
                '(1 2 3 4))

  (check-equal? (fringe (list x x))
                '(1 2 3 4 1 2 3 4)))
  
; exercise 2.29 binary mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)

(define (total-weight-branch b)
  (if (number? (branch-structure b))
      (branch-structure b)
      (total-weight-mobile (branch-structure b))))

(define (total-weight-mobile m)
  (+ (total-weight-branch (left-branch m))
     (total-weight-branch (right-branch m))))

(define (branch-torque b)
  (* (branch-length b) (total-weight-branch b)))

(define (mobile-balanced? m)
  (and
   (= (branch-torque (left-branch m))
      (branch-torque (right-branch m)))
   (and (or (number? (branch-structure (left-branch m)))
            (mobile-balanced? (branch-structure (left-branch m))))
        (or (number? (branch-structure (right-branch m)))
            (mobile-balanced? (branch-structure (right-branch m)))))))

(module+ test
  (require rackunit)

  ; overriding mobile
  (define (make-mobile left right)
    (cons left right))

  (define (make-branch len structure)
    (cons len structure))

  (check-equal?
   (total-weight-mobile (make-mobile (make-branch 1 2)
                                     (make-branch 1 3)))
   5)

  (check-equal?
   (total-weight-mobile (make-mobile (make-branch 1 (make-mobile
                                                     (make-branch 2 2)
                                                     (make-branch 3 3)))
                                     (make-branch 4 4)))
   9)

  (check-equal?
   (mobile-balanced? (make-mobile (make-branch 1 2)
                                  (make-branch 2 1)))
   #t)
  (check-equal?
   (mobile-balanced? (make-mobile (make-branch 1 3)
                                  (make-branch 2 1)))
   #f)
  (check-equal?
   (mobile-balanced? (make-mobile (make-branch 1 (make-mobile (make-branch 2 1)
                                                              (make-branch 1 2)))
                                  (make-branch 1 3)))
   #t)

)

;; exercise 2.32 distinct element generation
(define (subsets s) 
  (if (null? s) 
      (list '())
      (let ((rest (subsets (cdr s)))) 
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; exercise 2.33 implement list manipulations as accumulations
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (add1 y)) 0 sequence))


;; exercise 2.35 redefine count-leaves as an accumulation
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves-acc t)
  (accumulate +
              0
              (map (lambda (n)
                     (cond
                       [(null? n) 0]
                       [(pair? n) (count-leaves-acc n)]
                       [else 1]))
                   t)))

(module+ test
  (check-equal? (count-leaves-acc (list 1 2 3)) 3)
  (check-equal? (count-leaves-acc (list 1 2 3 (list 4 5))) 5)
)

;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(module+ test
  (check-equal? (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
                '(22 26 30))
)

; 2.39
(define (fold-right op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (fold-right op initial (cdr sequence))))) 
  
  
(define (fold-left op initial sequence) 
  (define (iter result rest) 
    (if (null? rest) 
        result 
        (iter (op result (car rest)) 
              (cdr rest)))) 
  (iter initial sequence))

(define (reverse-r sequence)
  (fold-right (lambda (item total)
                (append total (list item)))
              '() sequence))

(define (reverse-l sequence)
  (fold-left (lambda (total remaining)
               (append (list remaining) total))
             '() sequence))

(module+ test
  (check-equal? (reverse-r '(1 2 3))
                '(3 2 1))
  (check-equal? (reverse-l '(1 2 3))
                '(3 2 1))
) 

;; 2.40
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      '()
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (unique-pairs n)
  (flatmap
   (lambda (j)
     (map (lambda (i) (list i j))
          (range (+ j 1) (+ n 1))))
   (range 1 n)))

;; 2.41
(define (triples n target)
  "generate all ordered tripes of distinct integers i j k <= n and equal to s"
  (define (maybe-valid? ls)
    "determine if the set of values could fulfill s"
    (and (<= (length ls) 3)
         (<= (apply + ls) target)))
  (define (loop c ls)
    (if (> c n)
        ls
        (loop (add1 c) (append
                        (filter maybe-valid?
                                (map (lambda (l) (cons c l)) ls))
                        ls))))
  (filter (lambda (l)
            (and (= (length l) 3)
                 (= (apply + l) target)))
          (loop 2 '((1) ()))))
