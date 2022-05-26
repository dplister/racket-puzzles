#lang racket

(require "../prelude.rkt")

(define (pad-left lst n)
  "pads the start of lst n amount of 0's"
  (append (make-list (- n (length lst)) 0)
          lst))

(define (bin num)
  (pad-left (number->binary num) 36))

(define (parse-mask input)
  (map (lambda (v) (or (char->num v) v)) (string->list input)))

(define (parse-mem input)
  (map string->number (drop (regexp-match #px"mem\\[([0-9]+)\\] = ([0-9]+)" input) 1)))

(define (apply-mask mask bnum)
  (map (lambda (b m)
         (if (equal? m #\X) b m))
       bnum mask))

(define (apply-mask-split mask bnum [acc (list (list))])
  "applies the mask, generating a split set of values for each X encountered"
  (if (empty? mask) (map reverse acc)
      (apply-mask-split
       (rest mask)
       (rest bnum)
       (match (first mask)
         [0 (map (lambda (a) (cons (first bnum) a)) acc)]
         [1 (map (lambda (a) (cons 1 a)) acc)]
         [else (append-map (lambda (v)
                      (map (lambda (a) (cons v a)) acc))
                    (range 0 2))]))))

(define (parse-raw lines)
  (list (parse-mask (last (string-split (first lines))))
        (map parse-mem (drop lines 1))))

(define (run-program mask bnum [mem #hash()])
  (foldl
   (lambda (l r)
     (dict-set r
               (first l)
               (binary->number
                (apply-mask mask (bin (second l))))))
   mem
   bnum))

(define (run-program-2 mask bnum [mem #hash()])
  (foldl
   (lambda (bn outer-mem)
     (foldl (lambda (v inner-mem)
              (dict-set inner-mem
                        (binary->number v)
                        (cadr bn)))
            outer-mem
            (apply-mask-split mask (bin (car bn)))))
   mem
   bnum))
          

(define (part-a)
  (define subprogs
    (map parse-raw
         (sublists (file->lines "data/DayFourteen.txt")
                   (lambda (i) (regexp-match #rx"mask" i)))))
  (define full-mem
    (foldl (lambda (prog mem)
             (run-program (first prog) (second prog) mem))
           #hash() subprogs))
  (apply + (dict-values full-mem)))

(define (part-b)
  (define subprogs
    (map parse-raw
         (sublists (file->lines "data/DayFourteen.txt")
                   (lambda (i) (regexp-match #rx"mask" i)))))
  (define full-mem
    (foldl (lambda (prog mem)
             (run-program-2 (first prog) (second prog) mem))
           #hash() subprogs))
  (apply + (dict-values full-mem)))

(module+ test
  (require rackunit)

  (check-equal?
   (binary->number
    (apply-mask
     (parse-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
     (bin 11)))
   73)

  (define example-input
    (list "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
          "mem[8] = 11"
          "mem[7] = 101"
          "mem[8] = 0"))

  (check-equal?
   (second (parse-raw example-input))
   '((8 11) (7 101) (8 0)))

  (let ([r (parse-raw example-input)])
    (check-equal?
     (run-program (first r) (second r))
     #hash((7 . 101) (8 . 64))))

  (define (string->binary str)
    (map char->num (string->list str)))

  (check-equal?
   (apply-mask-split (parse-mask "01")
                     (list 1 0))
   '((1 1)))

  (check-equal?
   (apply-mask-split (parse-mask "01X")
                     (list 1 0 0))
   '((1 1 0)
     (1 1 1)))

  (check-equal?
   (apply-mask-split (parse-mask "XX")
                     (list 0 0))
   '((0 0)
     (1 0)
     (0 1)
     (1 1)))

  (check-equal?
        (apply-mask-split (parse-mask "000000000000000000000000000000X1001X")
                          (bin 42))
   (list (string->binary "000000000000000000000000000000011010")
         (string->binary "000000000000000000000000000000111010")
         (string->binary "000000000000000000000000000000011011")
         (string->binary "000000000000000000000000000000111011")))

  (define example-input-2
    (list "mask = 000000000000000000000000000000X1001X"
          "mem[42] = 100"
          "mask = 00000000000000000000000000000000X0XX"
          "mem[26] = 1"))

  (let ([r1 (parse-raw (take example-input-2 2))])
    (check-equal?
     (run-program-2 (first r1) (second r1))
     #hash((26 . 100) (27 . 100) (58 . 100) (59 . 100))))

  (let ([r2 (parse-raw (take (drop example-input-2 2) 2))])
    (check-equal?
     (run-program-2 (first r2) (second r2))
     #hash((16 . 1) (17 . 1) (18 . 1) (19 . 1) (24 . 1) (25 . 1) (26 . 1) (27 . 1))))
)
