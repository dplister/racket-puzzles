#lang racket

(provide (all-defined-out))

(require "prelude.rkt")

(struct point (x y) #:transparent)

(struct location point (z) #:transparent)

(struct grid (points width height) #:transparent)

(define (empty-grid)
  (grid #hash() 0 0))

(define (expand-grid g padding-amt default-value)
  "expands the grid by padding-amt in all directions, setting all new points' z to default-value"
  ; increase all current points
  (define eg
    (foldl (lambda (p g)
             (set-grid-coord g
                             (+ (point-x p) padding-amt)
                             (+ (point-y p) padding-amt)
                             (location-z p)))
           (grid #hash()
                 (+ (grid-width g) (* padding-amt 2))
                 (+ (grid-height g) (* padding-amt 2)))
           (dict-values (grid-points g))))
  (foldl (lambda (p g)
           (set-grid-coord g
                           (point-x p)
                           (point-y p)
                           default-value))
         eg
         (flatten (border eg padding-amt)))
  )

(define (all-points g)
  "returns all points in grid"
  (dict-values (grid-points g)))

(define (border g padding-amt)
  (flatten
   (append
   ; top
   (pfill 0 0 (grid-width g) padding-amt)
   ; left
   (pfill 0 
          padding-amt
          padding-amt
          (- (grid-height g) padding-amt padding-amt))
   ; right
   (pfill (- (grid-width g) padding-amt)
          padding-amt
          padding-amt
          (- (grid-height g) padding-amt padding-amt))
   ; bottom
    (pfill 0
           (- (grid-height g) padding-amt)
           (grid-width g)
           padding-amt)
    )))
  
(define (pfill x y w h)
  "generate rectangle set of points"
  (flatten
   (for/list ([ly (range y (+ y h))])
     (for/list ([lx (range x (+ x w))])
       (point lx ly)))))

(define (line g x y step-x step-y continue? #:include-break [include-break #f])
  "collects set of points starting at x y and continuing along step-x step-y until continue? is false"
  (define (loop cx cy)
    (let ([res (get-coord g cx cy)])
      (if (not (continue? res)) (if include-break (list res) (list))
          (cons res (loop (+ cx step-x) (+ cy step-y))))))
  (loop x y))

(define (string->numbers line)
  (map parse-number (string->list line)))

(define (parse-number c)
  (match c
    [#\1 1] [#\2 2] [#\3 3] [#\4 4] [#\5 5]
    [#\6 6] [#\7 7] [#\8 8] [#\9 9] [#\0 0]))

(define (parse-coords lst z-translation)
  (define mx (sub1 (string-length (first lst))))
  (define zs (string->list (apply string-append lst)))
  (define (loop l ps x y)
    (if (empty? l) ps
        (loop (rest l)
              (dict-set ps
                        (point x y)
                        (location x y (z-translation (first l))))
              (if (= x mx) 0 (add1 x))
              (if (= x mx) (add1 y) y))))
  (grid
   (loop zs #hash() 0 0)
   (+ mx 1)
   (length lst)))

(define (get-coord g x y #:wrap-x [wrap-x #f])
  (when (and wrap-x
             (>= x (grid-width g)))
    (set! x (wrap-number x (grid-width g))))
  (dict-ref (grid-points g) (point x y) #f))

(define (set-grid-coord g x y v)
  (grid (dict-set (grid-points g)
                  (point x y)
                  (location x y v))
        (max (add1 x) (grid-width g))
        (max (add1 y) (grid-height g))))

(define (surrounding g pt)
  (define x (point-x pt))
  (define y (point-y pt))
  (filter point?
          (map (lambda (p) (get-coord g (+ x (first p)) (+ y (second p))))
               (append cardinal-points orthogonal-points))))

(define (orthogonal g pt)
  (define x (point-x pt))
  (define y (point-y pt))
  (filter point?
          (map (lambda (p) (get-coord g (+ x (first p)) (+ y (second p))))
               cardinal-points)))

(define cardinal-points
  (list (list -1 0)  ; left
        (list 1 0)   ; right
        (list 0 -1)  ; up
        (list 0 1))) ; down

(define orthogonal-points
  (list (list -1 -1) ; upper-left
        (list -1 1)  ; lower-left
        (list 1 -1)  ; upper-right
        (list 1 1))) ; lower-right

(define (box-lines g pt width)
  "retrieves the set of points as a box of width size"
  (define stx (max 0 (- (point-x pt) width)))
  (define sty (max 0 (- (point-y pt) width)))
  (define enx (min (add1 (+ (point-x pt) width)) (grid-width g)))
  (define eny (min (add1 (+ (point-y pt) width)) (grid-height g)))
  (map (lambda (y)
                (filter-map (lambda (x) (get-coord g x y)) (range stx enx)))
              (range sty eny)))

(define (first-point g)
  (get-coord g 0 0))

(define (last-point g)
  (get-coord g (sub1 (grid-width g)) (sub1 (grid-height g))))

(define (format-grid g)
  (string-join
   (map (lambda (y)
          (apply string-append
                 (map (lambda (x)
                        (let ([p (get-coord g x y)])
                          (format "~a"
                                  (if (location? p)
                                      (location-z p)
                                      #\f))))
                      (range 0 (grid-width g)))))
        (range 0 (grid-height g)))
   "\n"))
  
; render testing
; (display (format-grid (parse-coords (list "123" "456"))))
;(display (format-grid (expand-grid (parse-coords (list "123" "456") parse-number) 2 0)))
;(display (format-grid (expand-grid (parse-coords (list "123" "456") parse-number) 1 0)))
;(box-lines (parse-mapped (list "12345" "67890" "abcde" "fghij" "klmno"))
;     (point 0 0 0) 1)

;(define tg (parse-coords (list "123" "456" "789") parse-number))
;(define bg (parse-coords (list "00000" "11111" "22222" "33333" "44444") parse-number))

(module+ test
  (require rackunit)

  (define small-grid
    (parse-coords (list "123" "456") parse-number))

  (for ([pos (list (list 0 0) (list 1 0) (list 2 0) (list 0 1) (list 1 1) (list 2 1))]
        [v (list 1 2 3 4 5 6)])
    (check-equal?
     (location-z (get-coord small-grid (first pos) (second pos)))
     v))

  ; test wrapping
  (check-equal?
   (location-z (get-coord small-grid 3 0 #:wrap-x #t))
   1)

   (check-equal?
    (first-point small-grid)
    (location 0 0 1))

   (check-equal?
    (last-point small-grid)
    (location 2 1 6))

   (define squares-map
     (parse-coords (list "00000" "01110" "01210" "01110" "00000") parse-number))

   ; can the box handle expanding beyond boundaries?
   (check-equal?
    (box-lines squares-map (point 0 0) 1)
    (list (list (location 0 0 0) (location 1 0 0))
          (list (location 0 1 0) (location 1 1 1))))

   ; full box
   (check-equal?
    (box-lines squares-map (point 1 1) 1)
    (list
     (list (location 0 0 0) (location 1 0 0) (location 2 0 0))
     (list (location 0 1 0) (location 1 1 1) (location 2 1 1))
     (list (location 0 2 0) (location 1 2 1) (location 2 2 2))))

   ; bigger box of width 2
   (check-equal?
    (box-lines squares-map (point 2 2) 2)
    (list
     (list (location 0 0 0) (location 1 0 0) (location 2 0 0) (location 3 0 0) (location 4 0 0))
     (list (location 0 1 0) (location 1 1 1) (location 2 1 1) (location 3 1 1) (location 4 1 0))
     (list (location 0 2 0) (location 1 2 1) (location 2 2 2) (location 3 2 1) (location 4 2 0))
     (list (location 0 3 0) (location 1 3 1) (location 2 3 1) (location 3 3 1) (location 4 3 0))
     (list (location 0 4 0) (location 1 4 0) (location 2 4 0) (location 3 4 0) (location 4 4 0))))

   ; line drawing
   (check-equal?
    (line squares-map 0 0 1 1 location?)
    (list (location 0 0 0)
          (location 1 1 1)
          (location 2 2 2)
          (location 3 3 1)
          (location 4 4 0)))
)
