#lang racket

(define (rpn tokens [stack (list)])
  (cond
    [(empty? tokens) stack]
    [(number? (first tokens))
     (rpn (rest tokens) (cons (first tokens) stack))]
    [else
     (rpn (rest tokens) (cons ((first tokens) (second stack) (first stack))
                              (drop stack 2)))]))

(define (loop [stack (list)])
  (println (format "Stack: ~a" stack))
  (let ([inp (read-line (current-input-port) 'any)])
    (if (string-locale-ci=? inp "q") stack
        (loop (rpn (parse-tokens inp) stack)))))

(define (parse-tokens input)
  (map (lambda (t) (parse-token t)) (string-split input)))

(define (parse-token t)
  (match t
    ["+" +]
    ["-" -]
    ["/" /]
    ["*" *]
    [(app string->number (? number? n))
     n]
    [else t]))

(module+ test
  (require rackunit)

  (check-equal? (rpn (list 1 1 +)) '(2))
  (check-equal? (real->decimal-string (first (rpn (list 19 2.14 + 4.5 2 4.3 / - *))) 4) "85.2974")

  (check-equal? (parse-tokens "1 2 + 3 /") (list 1 2 + 3 /))

)


