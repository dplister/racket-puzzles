#lang racket

(struct paren (opening closing opening-score closing-score) #:transparent)

(define parens
  (list (paren #\( #\) 3 1)
        (paren #\{ #\} 1197 3)
        (paren #\[ #\] 57 2)
        (paren #\< #\> 25137 4)))

(define (match-paren token f)
  (define matches (filter (lambda (v) (equal? (f v) token)) parens))
  (if (empty? matches) #f (first matches)))

(define (is-opening token)
  (match-paren token paren-opening))

(define (is-closing token)
  (match-paren token paren-closing))

; return
(define (first-invalid tokens [seen (list)])
  (cond
    ; termination cases
    [(and (empty? tokens) (empty? seen)) #f]
    [(empty? tokens) seen]
    [else
     ; processing the token
     (match (first tokens)
       [(app is-opening (? paren? opn))
        (first-invalid (rest tokens)
                       (cons (first tokens) seen))]
       [(app is-closing (? paren? cls))
        (if (or (empty? seen)
                (not (equal? (first seen) (paren-opening cls))))
            (first tokens)
            (first-invalid (rest tokens)
                           (rest seen)))])]))

(define (score-invalids lists)
  (define invalids
    (filter (lambda (v) (and (not (equal? v #f))
                             (not (list? v))))
            (map first-invalid lists)))
  (foldl + 0 (map paren-opening-score (chars->parens invalids))))

(define (score-missing ps [total 0])
  (if (empty? ps) total
      (score-missing (rest ps)
                     (+ (* total 5) (paren-closing-score (first ps))))))

(define (chars->parens ls)
  (map (lambda (v) (or (is-opening v)
                       (is-closing v)))
       ls))

(define (part-a)
  (define lines (map string->list (file->lines "data/DayTen.txt")))
  (score-invalids lines))

(define (part-b)
  (define lines (map string->list (file->lines "data/DayTen.txt")))
  (define incompletes
    (map chars->parens
         (filter (lambda (v) (list? v))
                 (map first-invalid lines))))
  (define scores (map score-missing incompletes))
  (list-ref (sort scores >) (quotient (length scores) 2)))

(module+ test
  (require rackunit)

  (check-equal?
   (is-opening #\[)
   (third parens))

  (define example-data
    (map string->list
         (list
          "[({(<(())[]>[[{[]{<()<>>"
          "[(()[<>])]({[<{<<[]>>("
          "{([(<{}[<>[]}>{[]{[(<()>"
          "(((({<>}<{<{<>}{[]{[]{}"
          "[[<[([]))<([[{}[[()]]]"
          "[{[{({}]{}}([{[{{{}}([]"
          "{<[[]]>}<{[{[{[]{()[[[]"
          "[<(<(<(<{}))><([]([]()"
          "<{([([[(<>()){}]>(<<{{"
          "<{([{{}}[<[[[<>{}]]]>[]]")))

  (define example-errors
    (list
     (list #\{ #\{ #\[ #\[ #\( #\{ #\( #\[)
     (list #\( #\{ #\< #\[ #\{ #\()
     #\}
     (list #\{ #\{ #\< #\{ #\< #\( #\( #\( #\()
     #\)
     #\]
     (list #\[ #\[ #\{ #\{ #\[ #\{ #\[ #\{ #\<)
     #\)
     #\>
     (list #\[ #\( #\{ #\<)))

  (for ([l example-data]
        [e example-errors])
    (check-equal? (first-invalid l) e))

  (check-equal?
   (score-invalids example-data)
   26397)

)
