#lang racket

(define (parse-op input)
  "converts a string line into an op"
  (match input
    [(regexp #rx"addx ([\\-]*[0-9]+)" (list _ num))
     (list 'add (string->number num))]
    ["noop"
     (list 'noop)]
    [else
      #f]))

(define (execute ops [val 1] [stack (list)])
  "executes each op and puts the result on the stack"
  (if (empty? ops) (reverse (cons val stack))
    (match (first ops)
      [(list 'add num)
       (execute (rest ops)
		(+ val num)
		(append (make-list 2 val) stack))]
      [(list 'noop)
       (execute (rest ops)
		val
		(cons val stack))])))

(define (fetch-signals ls)
  "gets the sequence of signals at first 20 and each 40 after"
  (cons (* 20 (list-ref ls 19))
	(map (lambda (i) (* (add1 i) (list-ref ls i)))
	     (range 59 (length ls) 40))))

(define (part-a input)
  (define ops (map parse-op input))
  (define signals (fetch-signals (execute ops)))
  (apply + signals))

(define (get-input)
  (file->lines "day_ten.txt"))

(part-a (get-input))

(define (render crt stack [cycle 1])
  "renders a set of pixels if crt is within stack values range"
  (if (>= cycle (length stack)) '()
    (cons 
      (rchar (list-ref stack crt) crt)
      (render (if (= crt 39) 0 (add1 crt)) stack (add1 cycle)))))

(define (rchar stack crt)
  "determines if the crt is in range of the stack value"
  (displayln (format "stack ~v crt ~v" stack crt))
  (if (and (>= crt (sub1 stack))
	   (<= crt (add1 stack)))
    #\# #\.))

(module+ test
  (require rackunit)

  (define example-1
    (list "noop"
	  "addx 3"
	  "addx -5"))

  (check-equal? (parse-op (first example-1)) (list 'noop))
  (check-equal? (parse-op (second example-1)) (list 'add 3))
  (check-equal? (parse-op (third example-1)) (list 'add -5))

  (check-equal?
    (execute (map parse-op example-1))
    '(1 1 1 4 4 -1))

  (define example-2
    (file->lines "day_ten.example"))

  (check-equal?
    (fetch-signals (execute (map parse-op example-2)))
    '(420 1140 1800 2940 2880 3960))

  (check-equal?
    (part-a example-2)
    13140)

  (define result-2
    (apply string-append (file->lines "day_ten.result")))

  (check-equal? 
    (list->string (render 0 (execute (map parse-op example-2))))
    result-2)
)
