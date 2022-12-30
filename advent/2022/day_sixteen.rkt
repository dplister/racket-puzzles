#lang racket

(define (parse-location input)
  "converts line of text into location description"
  (define tokens (rest (regexp-match #rx"Valve ([A-Z]+) has flow rate=([0-9]+); tunnel[s]? lead[s]? to valve[s]? (.+)" input)))
  (list
    (first tokens)
    (string->number (second tokens))
    (map string-trim (string-split (third tokens) ","))))

(define (location-map inputs)
  "creates a location map from the set of inputs"
  (define locs (make-hash))
  (for-each 
    (lambda (i) 
      (let ([loc (parse-location i)])
	(hash-set! locs (first loc) loc)))
    inputs)
  locs)

(define (explore current time dirs [attached (list)] [score 0])
  "continues adding valve locations until time runs out"
  (cond
    [(= time 0) score]
    [else
      (define additional-score (apply + (map second attached)))
      (apply max (append 
	       ; stay and open the valve
	       (if (and (valve-not-open? attached current)
			(pos-pressure-valve? dirs current))
		 (list (explore 
		   current 
		   (sub1 time) 
		   dirs 
		   (cons (hash-ref dirs current)
			 attached)
		   (+ score additional-score)))
		 '())
	       ; continue on without opening the valve
	       (map (lambda (d) 
		      (explore 
			d 
			(sub1 time)
			dirs
			attached
			(+ score additional-score)))
		    (third (hash-ref dirs current)))))
      ]))

(define (valve-not-open? dirs valve)
  "determines if the current valve is open"
  (not (member valve dirs (lambda (a b) (equal? a (first b))))))

(define (pos-pressure-valve? dirs valve)
  "determines if the current valve has pressure"
  (> (second (hash-ref dirs valve)) 0))

(module+ test
  (require rackunit)

  (define example
    (list "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
	  "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
	  "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
	  "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
	  "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
	  "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
	  "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
	  "Valve HH has flow rate=22; tunnel leads to valve GG"
	  "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
	  "Valve JJ has flow rate=21; tunnel leads to valve II"))

  (check-equal? (parse-location (first example))
		(list "AA" 0
		      (list "DD" "II" "BB")))

  (check-equal? (first (hash-ref (location-map example) "EE")) "EE")

  (check-equal? (explore "AA" 30 (location-map example))
		1651)
)
