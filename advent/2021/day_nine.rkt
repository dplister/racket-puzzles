#lang racket

(struct point (x y z) #:transparent)

(define (string->numbers line)
  (map parse-number (string->list line)))

(define (parse-number c)
  (match c
    [#\1 1] [#\2 2] [#\3 3] [#\4 4] [#\5 5]
    [#\6 6] [#\7 7] [#\8 8] [#\9 9] [#\0 0]))

(define (parse-coords lst)
  (list->vector (flatten (map string->numbers lst))))

(define (get-coord vec x y row-width)
  (cond
    [(< x 0) 'nil]
    [(> x (- row-width 1)) 'nil]
    [(< y 0) 'nil]
    [(>= (* y row-width) (vector-length vec)) 'nil]
    [else (point x y (vector-ref vec (+ x (* y row-width))))]))

(define (is-lowest? vec x y row-width)
  (define z (point-z (get-coord vec x y row-width)))
  (define less-than-z (lambda (p) (< z (point-z p))))
  (and
   (nil-or (get-coord vec (- x 1) y row-width) ; left
           less-than-z)
   (nil-or (get-coord vec (+ x 1) y row-width) ; right
           less-than-z)
   (nil-or (get-coord vec x (- y 1) row-width) ; up
           less-than-z) 
   (nil-or (get-coord vec x (+ y 1) row-width) ; down
           less-than-z)))

(define (orthogonal-dirs x y)
  (list
   (list (- x 1) y) ; left
   (list (+ x 1) y) ; right
   (list x (- y 1)) ; up
   (list x (+ y 1)))) ; down

(define (nil-or v f)
  (or (equal? v 'nil)
      (f v)))

(define (coords-height vec row-width)
  (/ (vector-length vec) row-width))

(define (find-all-lowest vec row-width)
  (filter (lambda (v) (point? v))
          (flatten 
           (for/list ([y (range 0 (coords-height vec row-width))])
             (for/list ([x (range 0 row-width)])
               (if (is-lowest? vec x y row-width)
                   (get-coord vec x y row-width)
                   'nil))))))

(define (local-depressions vec row-width source)
  (define z (point-z source))
  (define dirs (orthogonal-dirs (point-x source) (point-y source)))
  (define greater-than-z (lambda (p) (> (point-z p) z)))
  (define less-than-9 (lambda (p) (< (point-z p) 9)))
  ; explore all 4 areas around the source
  (filter (lambda (v) (not (equal? v 'nil)))
          (map (lambda (l)
                 (let ([p (get-coord vec (first l) (second l) row-width)])
                   (if (and (not (equal? p 'nil))
                            (greater-than-z p)
                            (less-than-9 p))
                       p 'nil)))
               dirs)))

(define (find-depressions vec row-width target [deps (list)] [remaining (list)])
  (define locals (local-depressions vec row-width target))
  (define new-locals
    (filter (lambda (p) (and (not (point-in deps p))
                             (not (point-in remaining p))))
            locals))
  (define new-remaining (append remaining new-locals))
  (if (empty? new-remaining)
      (cons target deps)
      (find-depressions vec row-width
                        (first new-remaining)
                        (cons target deps)
                        (rest new-remaining))))

(define (find-all-depressions vec row-width)
  (define lows (find-all-lowest vec row-width))
  (for/list ([l lows])
    (find-depressions vec row-width l)))

(define (basin-sizes vec row-width)
  (map length (find-all-depressions vec row-width)))

(define (point-in lst item)
  (not (empty? (filter (lambda (p) (equal? p item)) lst))))
                                   
(define (has-visited? source past)
  (not (empty? (filter (lambda (v) (equal? v source)) past))))

(define (part-a)
  (define lines (file->lines "data/DayNine.txt"))
  (define coords (parse-coords lines))
  (define lows (find-all-lowest coords (string-length (first lines))))
  (foldl + 0 (map (lambda (p) (+ 1 (point-z p))) lows)))

(define (part-b)
  (define lines (file->lines "data/DayNine.txt"))
  (define coords (parse-coords lines))
  (define row-width (string-length (first lines)))
  (define sizes (basin-sizes coords row-width))
  (apply * (take (sort sizes >) 3)))

(module+ test
  (require rackunit)

  (check-equal?
   (parse-coords (list "123" "456"))
   (list->vector (list 1 2 3 4 5 6)))

   (let ([vec (list->vector (list 1 2 3 4 5 6))])
     (for ([pos (list (list 0 0) (list 1 0) (list 2 0) (list 0 1) (list 1 1) (list 2 1))]
           [v (list 1 2 3 4 5 6)])
       (check-equal?
        (point-z (get-coord vec (first pos) (second pos) 3))
        v)))

  (check-equal?
   (is-lowest? (list->vector (list 1 2 3 1)) 0 0 2)
   #t)
  (check-equal?
   (is-lowest? (list->vector (list 1 2 3 1)) 1 1 2)
   #t)
  (check-equal?
   (is-lowest? (list->vector (list 1 2 3 1)) 0 1 2)
   #f)
  (check-equal?
   (is-lowest? (list->vector (list 1 2 3 1)) 1 0 2)
   #f)

  (define example-data
    (parse-coords
     (list
      "2199943210"
      "3987894921"
      "9856789892"
      "8767896789"
      "9899965678")))

  (check-equal?
   (is-lowest? example-data 1 0 10)
   #t)
  (check-equal?
   (is-lowest? example-data 9 0 10)
   #t)
  (check-equal?
   (is-lowest? example-data 2 2 10)
   #t)
  (check-equal?
   (is-lowest? example-data 6 4 10)
   #t)
  (check-equal?
   (is-lowest? example-data 6 3 10)
   #f)

  (check-equal?
   (find-all-lowest example-data 10)
   (list (point 1 0 1) (point 9 0 0)
         (point 2 2 5) (point 6 4 5)))

  (check-equal?
   (local-depressions example-data 10 (point 1 0 0))
   (list (point 0 0 2)))

  (check-equal?
   (find-depressions example-data 10 (point 1 0 0))
   (list (point 0 1 3) (point 0 0 2) (point 1 0 0)))

  (check-equal?
   (find-depressions example-data 10 (point 0 0 2))
   (list (point 0 1 3) (point 0 0 2)))

  (check-equal?
   (basin-sizes example-data 10)
   (list 3 9 14 9))
  
  )
