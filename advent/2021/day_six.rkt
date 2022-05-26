#lang racket

(define (get-comma-numbers line)
  (map string->number (string-split line ",")))

(define (step creatures)
  (define updated-creatures (make-vector (vector-length creatures)))
  (dict-for-each creatures
            (lambda (age amount)
              (cond
                [(= age 0)
                 (dict-set! updated-creatures 8 amount)
                 (dict-update! updated-creatures 6 (add-f amount) amount)]
                [else
                 (dict-update! updated-creatures (- age 1) (add-f amount) amount)])))
  updated-creatures)

(define (add-f n)
  (lambda (v) (+ n v)))

(define (initial-creatures lst max-age)
  (define creatures (make-vector (+ max-age 1)))
  (for ([c lst])
    (dict-update! creatures c add1 1))
  creatures)

(define (step-n creatures n)
  (if (= n 0) creatures
      (step-n
       (step creatures)
       (- n 1))))

(define (total-creatures v)
  (foldl + 0 (dict-values v)))

(define (part-a)
  (define ages (get-comma-numbers (first (file->lines "data/DaySix.txt"))))
  (define initial (initial-creatures ages 8))
  (total-creatures (step-n initial 80)))

(define (part-b)
  (define ages (get-comma-numbers (first (file->lines "data/DaySix.txt"))))
  (define initial (initial-creatures ages 8))
  (total-creatures (step-n initial 256)))

(module+ test
  (require rackunit)

  (define example-ages (list 3 4 3 1 2))

  (check-equal?
   (initial-creatures example-ages 4)
   #(0 1 1 2 1))

  (check-equal?
   (step (initial-creatures example-ages 8))
   #(1 1 2 1 0 0 0 0 0))

  (check-equal?
   (step (step (initial-creatures example-ages 8)))
   #(1 2 1 0 0 0 1 0 1))

  ; checking the 18th iteration from example
  (check-equal?
   (step-n (initial-creatures example-ages 8) 18)
   (initial-creatures
    (get-comma-numbers "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8") 8))

  (check-equal?
   (total-creatures (initial-creatures example-ages 4))
   5)

  (check-equal?
   (total-creatures (step-n (initial-creatures example-ages 8) 18))
   26)

  (check-equal?
   (total-creatures (step-n (initial-creatures example-ages 8) 80))
   5934)

  (check-equal?
   (total-creatures (step-n (initial-creatures example-ages 8) 256))
   26984457539)
)
