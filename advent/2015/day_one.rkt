#lang racket

(define fc (hash #\( 1 #\) -1))

(define (floors input)
  (apply + 
	 (map (lambda (c) (hash-ref fc c))
	      (string->list input))))

(define (part-a)
  (apply + 
	 (map (lambda (line) (floors line)) 
	      (file->lines "day_one.txt"))))

(part-a)

(define (count-movements ls [n 0] [p 0])
  (if (= p -1) n
    (count-movements (cdr ls) 
		     (add1 n)
		     (+ p (hash-ref fc (car ls))))))

(define (part-b)
  (count-movements (string->list (apply append (file->lines "day_one.txt")))))

(part-b)

(module+ test
  (require rackunit)

  (check-equal? (floors "(())") 0)
  (check-equal? (floors "()()") 0)
  (check-equal? (floors "(((") 3)
  (check-equal? (floors "(()(()(") 3)
  (check-equal? (floors "))(((((") 3)
  (check-equal? (floors "())") -1)
  (check-equal? (floors "))(") -1)
  (check-equal? (floors ")))") -3)
  (check-equal? (floors ")())())") -3)

  (check-equal? (count-movements (string->list ")")) 1)
  (check-equal? (count-movements (string->list "()())")) 5)

)
