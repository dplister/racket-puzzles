#lang racket

(struct bingo-result (last-number winning-card) #:transparent)

(define (get-bingos lines)
  (if (empty? lines) (list)
      (cons (get-bingo (take (drop lines 1) 5))
            (get-bingos (drop lines 6)))))

(define (get-bingo lines)
  (map get-space-numbers lines))
   
(define (get-comma-numbers line)
  (map string->number (string-split line ",")))

(define (get-space-numbers line)
  (map string->number (string-split line)))

(define (mark-number lines num)
  (map (lambda (l)
         (map (lambda (x)
                (if (and
                     (number? num)
                     (equal? x num))
                    'checked x)) l))
       lines))

(define (has-bingo? lines)
  (or (first-t has-bingo-row? (zip-lists lines)) ;  columns
      (first-t has-bingo-row? lines))) ; rows

(define (has-bingo-row? line)
  (empty? (filter (lambda (n) (number? n)) line)))

(define (first-t f lst)
  (if (empty? lst) #f
      (or (f (first lst))
          (first-t f (rest lst)))))

(define (zip-lists lst)
  (if (or (empty? lst) (empty? (first lst)))
      (list)
      (cons (map first lst)
            (zip-lists (map rest lst)))))

(define (mark-bingos cards numbers)
  (define marked (map (lambda (l) (mark-number l (first numbers))) cards))
  (define bingos (filter has-bingo? marked))
  (if (not (empty? bingos))
      (bingo-result (first numbers) (first bingos))
      (mark-bingos marked (rest numbers))))

(define (mark-bingos-until-last cards numbers)
  (define marked (map (lambda (l) (mark-number l (first numbers))) cards))
  (define bingos (filter has-bingo? marked))
  (define not-bingos (filter (lambda (m) (not (has-bingo? m))) marked))
  (if (empty? not-bingos)
      (bingo-result (first numbers) (first bingos))
      (mark-bingos-until-last not-bingos (rest numbers))))

(define (run lines)
  (define numbers (get-comma-numbers (first lines)))
  (define cards (get-bingos (drop lines 1)))
  (mark-bingos cards numbers))

(define (run-until-last lines)
  (define numbers (get-comma-numbers (first lines)))
  (define cards (get-bingos (drop lines 1)))
  (mark-bingos-until-last cards numbers))

(define (sum-bingo lines)
  (foldl + 0
         (map (lambda (n) (if (not (number? n)) 0 n))
              (flatten lines))))

(define (part-a)
  (define lines (file->lines "data/DayFour.txt"))
  (define result (run lines))
  (* (bingo-result-last-number result)
     (sum-bingo (bingo-result-winning-card result))))

(define (part-b)
  (define lines (file->lines "data/DayFour.txt"))
  (define result (run-until-last lines))
  (* (bingo-result-last-number result)
     (sum-bingo (bingo-result-winning-card result))))

(module+ test
  (require rackunit)

  (struct bingo-test (expected cards))

  (define bingo-cards
    (list (bingo-test #f (list (list 'checked 1) (list 2 3))) ; only one
          (bingo-test #f (list (list 'checked 1) (list 2 'checked))) ; no diagonal bingos
          (bingo-test #t (list (list 'checked 1) (list 'checked 2))) ; vertical bingo
          (bingo-test #t (list (list 1 2) (list 'checked 'checked))) ; horizontal bingo
          ))

  (for ([bt bingo-cards])
    (check-equal? (bingo-test-expected bt)
                  (has-bingo? (bingo-test-cards bt))))

  (define example-cards
    (list
     (list
      (list 22 13 17 11 0)
      (list 8  2 23  4 24)
      (list 21  9 14 16  7)
      (list 6 10  3 18  5)
      (list 1 12 20 15 19))
     
     (list
      (list 3 15  0  2 22)
      (list 9 18 13 17  5)
      (list 19  8  7 25 23)
      (list 20 11 10 24  4)
      (list 14 21 16 12  6))

     (list
      (list 14 21 17 24  4)
      (list 10 16 15  9 19)
      (list 18  8 23 26 20)
      (list 22 11 13  6  5)
      (list  2  0 12  3  7))))
  
  (define example-numbers
    (list 7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))

  (define example-result
    (bingo-result
     24
     (list
      (list 'checked 'checked 'checked 'checked 'checked)
      (list 10 16 15 'checked 19)
      (list 18 8 'checked 26 20)
      (list 22 'checked 13 6 'checked)
      (list 'checked  'checked 12 3  'checked))))

  (check-equal? example-result (mark-bingos example-cards example-numbers))

  (let ([result (mark-bingos example-cards example-numbers)])
    (check-equal? 24 (bingo-result-last-number result))
    (check-equal? 188 (sum-bingo (bingo-result-winning-card result))))

  
  (let ([result (mark-bingos-until-last example-cards example-numbers)])
    (check-equal? 13 (bingo-result-last-number result))
    (check-equal? 148 (sum-bingo (bingo-result-winning-card result))))
)
