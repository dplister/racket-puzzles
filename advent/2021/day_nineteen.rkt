#lang racket

(require "prelude.rkt")

(define (parse-scanners lines)
  (map parse-scanner
       (sublists lines (lambda (l) (string-prefix? l "--- scanner")))))

(define (parse-scanner lines)
  (define cl (takef (rest lines) (lambda (l) (> (string-length l) 0))))
  (map parse-coords cl))

(define (parse-coords l)
  (map string->number (string-split l ",")))

(define (generate-coord-combos lst)
   

(module+ test
  (require rackunit)

  (define example-input
    (file->lines "data/DayNinteenExample.txt"))

  (check-equal?
   (second (parse-scanners example-input))
   (list (list 686 422 578)
         (list 605 423 415)
         (list 515 917 -361)
         (list -336 658 858)
         (list 95 138 22)
         (list -476 619 847)
         (list -340 -569 -846)
         (list 567 -361 727)
         (list -460 603 -452)
         (list 669 -402 600)
         (list 729 430 532)
         (list -500 -761 534)
         (list -322 571 750)
         (list -466 -666 -811)
         (list -429 -592 574)
         (list -355 545 -477)
         (list 703 -491 -529)
         (list -328 -685 520)
         (list 413 935 -424)
         (list -391 539 -444)
         (list 586 -435 557)
         (list -364 -763 -893)
         (list 807 -499 -711)
         (list 755 -354 -619)
         (list 553 889 -390)))

)
