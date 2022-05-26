#lang racket

(require "prelude.rkt")

(struct packet (ver typ len val) #:transparent)

(define hexbins
  '((#\0 (0 0 0 0))
    (#\1 (0 0 0 1))
    (#\2 (0 0 1 0))
    (#\3 (0 0 1 1))
    (#\4 (0 1 0 0))
    (#\5 (0 1 0 1))
    (#\6 (0 1 1 0))
    (#\7 (0 1 1 1))
    (#\8 (1 0 0 0))
    (#\9 (1 0 0 1))
    (#\A (1 0 1 0))
    (#\B (1 0 1 1))
    (#\C (1 1 0 0))
    (#\D (1 1 0 1))
    (#\E (1 1 1 0))
    (#\F (1 1 1 1))))

(define (hex->bin c)
  (second (first (filter (lambda (kv) (equal? (car kv) c)) hexbins))))

(define (bin->hex l)
  (let ([lx (pad-bits l 4)])
    (first (first (filter (lambda (kv) (equal? (cadr kv) lx)) hexbins)))))

(define (decode-hex input)
  (flatten (map hex->bin (string->list input))))

(define (pad-bits xs l)
  (if (< (length xs) l)
      (pad-bits (cons 0 xs) l)
      xs))

(define (decode-packet bin)
  (define typ (char->num (bin->hex (take (drop bin 3) 3))))
  (define-values (l v) (decode-action typ (drop bin 6)))
  (packet (char->num (bin->hex (take bin 3)))
          typ (+ l 6) v))

(define (decode-action typ bin)
  (match typ
    [4 (decode-literal bin)]
    [else (decode-operator bin)]))

(define (decode-literal bin [acc (list)] [acc-len 0])
  (let ([rev-acc (append acc (take (drop bin 1) 4))])
    (if (= (first bin) 0)
        (values (+ acc-len 5)
                (binary->number rev-acc))
        (decode-literal (drop bin 5) rev-acc (+ acc-len 5)))))

(define (decode-operator bin)
  (match (first bin)
    [0 (let ([expected-len (binary->number (take (drop bin 1) 15))])
         (let-values ([(l r)
                       (decode-packets (drop bin 16)
                                       (lambda (ps) (> expected-len (packet-total-length ps))))])
           (values (+ l 16) r)))]
    [1 (let ([expected-amt (binary->number (take (drop bin 1) 11))])
         (let-values ([(l r)
                       (decode-packets (drop bin 12)
                                       (lambda (ps) (> expected-amt (length ps))))])
           (values (+ l 12) r)))]))

(define (decode-packets bin packets-left? [acc (list)])
  (if (not (packets-left? acc))
      (values (packet-total-length acc) (reverse acc))
      (let ([p (decode-packet bin)])
        (decode-packets (drop bin (packet-len p)) packets-left? (cons p acc)))))

(define (packet-total-length ps)
  (foldl (lambda (l r) (+ r (packet-len l))) 0 ps))

(define (sum-versions p)
  (+ (packet-ver p)
     (if (list? (packet-val p))
         (foldl + 0 (map sum-versions (packet-val p)))
         0)))

(define (execute-packet p)
  (if (= (packet-typ p) 4)
      (packet-val p)
      (let ([pvs (map execute-packet (packet-val p))])
        (match (packet-typ p)
          [0 (foldl + 0 pvs)]
          [1 (foldl * 1 pvs)]
          [2 (apply min pvs)]
          [3 (apply max pvs)]
          [5 (if (> (first pvs) (second pvs)) 1 0)]
          [6 (if (< (first pvs) (second pvs)) 1 0)]
          [7 (if (= (first pvs) (second pvs)) 1 0)]))))

(define (part-a)
  (define input (string-trim (file->string "data/DaySixteen.txt")))
  (sum-versions (decode-packet (decode-hex input))))

(define (part-b)
  (define input (string-trim (file->string "data/DaySixteen.txt")))
  (execute-packet (decode-packet (decode-hex input))))

(module+ test
  (require rackunit)

  (check-equal?
   (hex->bin #\0)
   (list 0 0 0 0))

  (check-equal?
   (decode-hex "A")
   (list 1 0 1 0))

  (check-equal?
   (decode-hex "AF")
   (list 1 0 1 0 1 1 1 1))

  (check-equal?
   (bin->hex (list 1 1 1 0))
   #\E)

  ; ensure shortest bit amount still matches to correct hex
  (check-equal?
   (bin->hex (list 1 1 1))
   #\7)

  (check-equal?
   (decode-hex "D2FE28")
   (list 1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0))

   (let ([p (decode-packet
             (list 1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0))])
     (check-equal?
      (packet-ver p)
      6)
     (check-equal?
      (packet-typ p)
      4)
     (check-equal?
      (packet-len p)
      21)
     (check-equal?
      (packet-val p)
      2021))

   (let-values ([(l v) (decode-literal (list 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0))])
     (check-equal? v 2021)
     (check-equal? l 15))

   (define example-op-literals
     (list 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0))
   
   (check-equal?
    (decode-packet example-op-literals)
    (packet 1 6 (- (length example-op-literals) 7) ; the 7 on the end are just junk
            (list (packet 6 4 11 10)
                  (packet 2 4 16 20))))

   (define example-op-three-literals
     (list 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0))

   (check-equal?
    (decode-packet example-op-three-literals)
    (packet 7 3 (- (length example-op-three-literals) 5) ; the 5 on the end are just junk
            (list (packet 2 4 11 1)
                  (packet 4 4 11 2)
                  (packet 1 4 11 3))))

   (check-equal?
    (sum-versions (packet 1 6 0
                          (list (packet 6 4 11 10)
                                (packet 2 4 16 20))))
    9)

   (check-equal?
    (sum-versions (packet 7 3 0
                          (list (packet 2 4 11 1)
                                (packet 4 4 11 2)
                                (packet 1 4 11 3))))
    14)

   ; further examples
   (check-equal?
    (sum-versions (decode-packet (decode-hex "8A004A801A8002F478")))
    16)
   (check-equal?
    (sum-versions (decode-packet (decode-hex "620080001611562C8802118E34")))
    12)
   (check-equal?
    (sum-versions (decode-packet (decode-hex "C0015000016115A2E0802F182340")))
    23)
   (check-equal?
    (sum-versions (decode-packet (decode-hex "A0016C880162017C3686B18A3D4780")))
    31)

   ; part b examples
   (check-equal?
    (execute-packet (decode-packet (decode-hex "C200B40A82")))
    3)
   (check-equal?
    (execute-packet (decode-packet (decode-hex "04005AC33890")))
    54)
   (check-equal?
    (execute-packet (decode-packet (decode-hex "880086C3E88112")))
    7)
   (check-equal?
    (execute-packet (decode-packet (decode-hex "CE00C43D881120")))
    9)
   (check-equal?
    (execute-packet (decode-packet (decode-hex "D8005AC2A8F0")))
    1)
   (check-equal?
    (execute-packet (decode-packet (decode-hex "F600BC2D8F")))
    0)
   (check-equal?
    (execute-packet (decode-packet (decode-hex "9C005AC2F8F0")))
    0)
   (check-equal?
    (execute-packet (decode-packet (decode-hex "9C0141080250320F1802104A08")))
    1)

   
)
