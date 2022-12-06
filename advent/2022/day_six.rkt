#lang racket

(define (find-marker input len)
  "finds the index of the first set of unique characters of length len in input"
  (define cs (string->list input))
  (define (loop l acc index)
    (let ([dupe (check-duplicates acc)])
      (cond
	[(equal? dupe #f) index]
	[else 
	  (let ([skip (+ 1 (index-of acc dupe))])
	    ; (displayln (format "Found duplicate at ~v of ~v" skip dupe))
	    (loop (drop l skip) 
		  (append (drop acc skip) (take l skip))
		  (+ index skip)))])))
  (loop (drop cs len) (take cs len) len))

(define (get-input)
  (file->string "day_six.txt"))

(define (part-a)
  (find-marker (get-input) 4))

(part-a)

(define (part-b)
  (find-marker (get-input) 14))

(part-b)

(module+ test
  (require rackunit)

  (check-equal? (find-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4) 7)
  (check-equal? (find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 4) 5)
  (check-equal? (find-marker "nppdvjthqldpwncqszvftbrmjlhg" 4) 6)
  (check-equal? (find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4) 10)
  (check-equal? (find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4) 11)

  (check-equal? (find-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14) 19)
  (check-equal? (find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 14) 23)
  (check-equal? (find-marker "nppdvjthqldpwncqszvftbrmjlhg" 14) 23)
  (check-equal? (find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14) 29)
  (check-equal? (find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14) 26)

)
