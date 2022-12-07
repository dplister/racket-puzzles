#lang racket

(struct node (name files dirs))

(define (parse-line input)
  (match input
    [(regexp #rx"^\\$ cd (.+)" (list _ dir))
     (list 'move dir)]
    [(regexp #rx"^\\$ ls" (list _))
     (list 'ls)]
    [(regexp #rx"^dir (.+)" (list _ dirname))
     (list 'dir dirname)]
    [(regexp #rx"^([0-9]+) (.+)" (list _ size filename))
     (list 'file filename (string->number size))]))

(define (execute moves [paths (list)]))

(module+ test
  (require rackunit)

  (check-equal? (parse-line "$ cd /") (list 'move "/"))
  (check-equal? (parse-line "$ ls") (list 'ls))
  (check-equal? (parse-line "dir d") (list 'dir "d"))
  (check-equal? (parse-line "62596 h.lst") (list 'file "h.lst" 62596))

)
