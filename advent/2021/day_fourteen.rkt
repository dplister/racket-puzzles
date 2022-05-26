#lang racket

(define (parse-poly input)
  (define tokens (string-split input " -> "))
  (list (first tokens)
        (second tokens)))

(define (get-template key templates)
  (dict-ref templates key))

(define (hash-poly lines)
  (define h (make-hash))
  (for ([l lines])
    (let ([p (parse-poly l)])
      (dict-set! h (first p) (second p))))
  h)

(define (hash-template input [hs #hash()])
  (if (or (not (non-empty-string? input))
          (= (string-length input) 1))
      hs
      (hash-template (substring input 1)
                     (dict-update hs (substring input 0 2) add1 0))))

(define (memo-template pairs templates)
  (define updated (make-hash))
  (dict-for-each pairs
                 (lambda (k v)
                   (dict-update! updated
                                 (string-append (string (string-ref k 0))
                                                (get-template k templates))
                                 (lambda (lv) (+ lv v)) 0)
                   (dict-update! updated
                                 (string-append (get-template k templates)
                                                (string (string-ref k 1)))
                                 (lambda (lv) (+ lv v)) 0)))
  updated)
                   
(define (memo-template-n pairs templates n)
  (if (= n 0) pairs
      (memo-template-n
       (memo-template pairs templates)
       templates
       (- n 1))))

(define (count-memos pairs)
  (define h (make-hash))
  (dict-for-each pairs
                 (lambda (k v)
                   (dict-update! h (string-ref k 0)
                                 (lambda (lv) (+ lv v)) 0)))
  h)

(define (memo-distance input)
  (define ec (count-memos input))
  (define mx (first (sort (dict-values ec) >)))
  (define mn (first (sort (dict-values ec) <)))
  (add1 (- mx mn)))

; note: there is a strange bug where ("NF" . 2) is stuck in the final hashmap regardless of size

(define (part-a)
  (define lines (file->lines "data/DayFourteen.txt"))
  (define input (first lines))
  (define polies (hash-poly (drop lines 2)))
  (- (memo-distance
      (memo-template-n (hash-template input) polies 10))
     2))

(define (part-b)
  (define lines (file->lines "data/DayFourteen.txt"))
  (define input (first lines))
  (define polies (hash-poly (drop lines 2)))
  (- (memo-distance
      (memo-template-n (hash-template input) polies 40))
     2))

(module+ test
  (require rackunit)

  (define example-polies
    (hash-poly
         (list "CH -> B"
               "HH -> N"
               "CB -> H"
               "NH -> C"
               "HB -> C"
               "HC -> B"
               "HN -> C"
               "NN -> C"
               "BH -> H"
               "NC -> B"
               "NB -> B"
               "BN -> B"
               "BB -> N"
               "BC -> B"
               "CC -> N"
               "CN -> C")))

  (define starting-input "NNCB")

  (check-equal?
   (memo-distance (memo-template-n (hash-template starting-input) example-polies 10))
   1588)

  (check-equal?
   (memo-distance (memo-template-n (hash-template starting-input) example-polies 40))
   2188189693529)
   
)
