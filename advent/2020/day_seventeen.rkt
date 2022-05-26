#lang racket

(require "../prelude.rkt")


(define (parse-state lines [padding (list 0)])
  (define (loop-line x y l [acc (list)])
    (if (empty? l) acc
        (loop-line (add1 x) y (rest l)
                   (if (equal? (first l) #\#)
                       (cons (append (list x y) padding) acc)
                       acc))))
  (define (loop y ls [acc (list)])
    (if (empty? ls)
        acc
        (loop (add1 y) (rest ls)
              (loop-line 0 y (first ls) acc))))
  (loop 0 lines))

(define (neighbour-counts poses)
  (define (loop ps ds)
    (if (empty? ps) ds
        (let ([nc (pos-neighbours (first ps))])
          ; (displayln (format "ps: ~v neighbours: ~v" (first ps) nc))
          (loop
           (rest ps)
           (foldl (lambda (n res) (dict-update res n add1 0)) ds nc)))))
  (loop poses #hash()))

(define (step ps)
  "from initial set of positions, generate the next set"
  (define ncs (neighbour-counts ps))
  (filter-map
   (lambda (n)
     (and (or (and (= (cdr n) 2) (member (car n) ps))
              (= (cdr n) 3))
          (car n)))
   (dict->list ncs)))

(define (cycle ps n)
  "from initial set of positions, step n times"
  (if (= n 0) ps
      (cycle (step ps) (sub1 n))))

(define sort-pos
  (lambda (p1 p2)
    (or-list < p1 p2)))

(define (pos-neighbours p)
  "generates set of neighbour points"
  (map (lambda (perm)
         (map (lambda (v pm)
                (+ v pm))
              p perm))
       (filter (lambda (perm) (not (apply = 0 perm)))
               (perm-sublists (make-list (length p) '(-1 0 1)) #:dupes #t))))
  
; generate all neighbours, and count the duplicates for logic
(define (part-a)
  (define initial (parse-state (map string->list (file->lines "data/DaySeventeen.txt"))))
  (length (cycle initial 6)))

(define (part-b)
  (define initial (parse-state (map string->list (file->lines "data/DaySeventeen.txt")) (list 0 0)))
  (length (cycle initial 6)))

(define (collect-mins-maxes ls)
  (define (loop ls acc)
    (if (or (empty? ls) (empty? (first ls)))
        (reverse acc)
        (loop (map rest ls)
              (cons (cons (min-f (map first ls) identity)
                          (max-f (map first ls) identity))
                    acc))))
  (loop ls (list)))
  
(define (render-points ps)
  (define (loop ls mnxs)
    (let ([start (car (first mnxs))]
          [end (add1 (cdr (first mnxs)))])
      (if (= (length mnxs) 1)
          (for ([v (range start end)])
            (display (if (member (cons v ls) ps) "#" ".")))
          (for ([v (range start end)])
            (displayln "")
            (loop (cons v ls) (rest mnxs))))))
  (define mnxs (collect-mins-maxes ps))
  (loop (list) (reverse mnxs)))

(define example-a
  (map string->list
       (list ".#."
             "..#"
             "###")))

; (render-points (parse-state example-a))
;(render-points (step (parse-state example-a)))
; (render-points (cycle (parse-state example-a) 3))

(module+ test
  (require rackunit)

  (check-equal?
   (sort (parse-state example-a) sort-pos)
   '((1 0 0)
     (2 1 0)
     (0 2 0)
     (1 2 0)
     (2 2 0)))
  
  (let ([nc (pos-neighbours '(1 2 3))])
    (check-equal?
     (not (empty? (member '(2 2 2) nc)))
     #t)
    (check-equal?
     (not (empty? (member '(0 2 3) nc)))
     #t))

  (check-equal?
   (length (cycle (parse-state example-a) 6))
   112)

  (check-equal?
   (length (cycle (parse-state example-a (list 0 0)) 6))
   848)

)
