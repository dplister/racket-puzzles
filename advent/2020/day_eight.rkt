#lang racket

(define (parse-instruction input)
  (let ([tokens (string-split input)])
    (cons (match (first tokens)
            ["nop" 'nop]
            ["acc" 'acc]
            ["jmp" 'jmp])
          (string->number (second tokens)))))

(define (parse-instructions input)
  (list->vector (map parse-instruction input)))

(define (execute instructions)
  (define (loop visited index acc)
    (cond
      [(>= index (vector-length visited))
       (cons 'ended acc)]
      [(equal? (vector-ref visited index) 1)
       (cons 'infinite-loop acc)]
      [else
       (vector-set! visited index 1)
       (match (vector-ref instructions index)
          [(cons 'nop num)
           (loop visited (add1 index) acc)]
          [(cons 'acc num)
           (loop visited (add1 index) (+ acc num))]
          [(cons 'jmp num)
           (loop visited (+ index num) acc)])]))
  (loop (make-vector (vector-length instructions) 0) 0 0))

(define (next-op instructions index ops)
  "returns the position of the next post-index op that matches one of the list of ops"
  (define (loop i)
    (if (member (car (vector-ref instructions i)) ops)
        i
        (loop (add1 i))))
  (loop (add1 index)))

(define (fix-and-execute instructions)
  (define ops '(jmp op))
  (define (flip-instruction ins)
    (cons
     (match (car ins)
       ['nop 'jmp]
       ['jmp 'nop])
     (cdr ins)))
  (define (loop swap-index)
    (let ([upd-instr (vector-copy instructions)])
      (vector-set! upd-instr
                   swap-index
                   (flip-instruction (vector-ref instructions swap-index)))
      (let ([result (execute upd-instr)])
        (if (equal? (car result) 'ended) result
            (loop (next-op instructions swap-index ops))))))
  (loop (next-op instructions -1 ops)))
  
(define (part-a)
  (define instructions (parse-instructions (file->lines "data/DayEight.txt")))
  (execute instructions))

(define (part-b)
  (define instructions (parse-instructions (file->lines "data/DayEight.txt")))
  (fix-and-execute instructions))

(module+ test
  (require rackunit)

  (for ([input (list "nop +0" "acc +1" "jmp +4" "acc -99")]
        [expected '((nop . 0) (acc . 1) (jmp . 4) (acc . -99))])
    (check-equal?
     (parse-instruction input)
     expected))

  (check-equal?
   (execute (list->vector '((nop . 0) (acc . 2) (jmp . -1))))
   '(infinite-loop . 2))

  (define example-data
    (list "nop +0"
          "acc +1"
          "jmp +4"
          "acc +3"
          "jmp -3"
          "acc -99"
          "acc +1"
          "jmp -4"
          "acc +6"))

  (check-equal?
   (execute (parse-instructions example-data))
   '(infinite-loop . 5))

  (check-equal?
   (fix-and-execute (parse-instructions example-data))
   '(ended . 8))
    
)
