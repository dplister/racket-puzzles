#lang racket

(define (get-numbers filepath)
  (map string->number (file->lines filepath)))

(define (count-increases nums window)
  (apply + 
   (map (lambda (index)
          (if (< (apply + (take (drop nums (- index window)) window))
                 (apply + (take (drop nums (- index (- window 1))) window)))
              1 0))
        (range window (length nums)))))

(define (part-a)
  (count-increases (get-numbers "data/DayOne.txt") 1))

(define (part-b)
  (count-increases (get-numbers "data/DayOne.txt") 3))

