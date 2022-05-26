#lang racket

(require "prelude.rkt")

(define (parse-binary input)
  (map
   (lambda (c)
     (match c
       [#\0 0]
       [#\1 1]))
   (string->list input)))

(define (common-bit bins)
  (define total (foldr + 0 (map car bins)))
  (define remainder (- (length bins) total))
  (cond
    [(< remainder total) 1]
    [(> remainder total) 0]
    [else 'equal]))

(define (common-bits bins resolver)
  (if (or (empty? bins) (empty? (first bins)))
      (list)
      (cons (resolver (common-bit bins))
            (common-bits (map rest bins) resolver))))

(define (common-filtered-bits bins resolver)
  (cond
    [(or (empty? bins)
         (empty? (first bins))) (list)]
    [(= (length bins) 1) (first bins)]
    [else 
      (define b (resolver (common-bit bins)))
      (cons b (common-filtered-bits (map rest (bit-filter bins b)) resolver))]))

(define (bit-filter bins target)
  (filter (lambda (b) (= target (car b))) bins))

(define (most-significant v)
  (match v
    [1 1]
    [0 0]
    ['equal 1]))

(define (least-significant v)
  (match v
    [1 0]
    [0 1]
    ['equal 0]))
                  
(define (part-a)
  (define input (map parse-binary (file->lines "data/DayThree.txt")))
  (define gamma (common-bits input most-significant))
  (define epsilon (common-bits input least-significant))
  (* (binary->number gamma) (binary->number epsilon)))

(define (part-b)
  (define input (map parse-binary (file->lines "data/DayThree.txt")))
  (define oxygen (common-filtered-bits input most-significant))
  (define co2 (common-filtered-bits input least-significant))
  (* (binary->number oxygen) (binary->number co2)))
  
