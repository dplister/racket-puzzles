#lang racket

(struct node (left right) #:transparent #:mutable)

(define numchars
  (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (snail-num input)
  (takef input
         (lambda (v)
           (member v numchars))))

(define (parse-snail-string input)
  (parse-snail (string->list input)))

(define (parse-snail input)
  (define working-input (drop-right (rest input) 1)) ; eat outer brackets
  (define leaves
    (map
     (lambda (v)
       (when (equal? (first working-input) #\,)
           (set! working-input (drop working-input 1)))
       (cond
         [(equal? (first working-input) #\[)
          (let ([sbl (substring-brackets working-input)])
            (set! working-input (drop working-input (length sbl)))
            (parse-snail sbl))]
         [else
          (let ([snl (snail-num working-input)])
            (set! working-input (drop working-input (length snl)))
            (string->number (list->string snl)))]))
     (range 0 2)))
  (node (first leaves)
        (second leaves)))

(define (substring-brackets input [brackets (list)] [acc (list)])
  (cond
    [(and (not (empty? acc))
          (empty? brackets))
     (reverse acc)]
    [(equal? (first input) #\[)
     (substring-brackets (rest input)
                         (cons #\[ brackets)
                         (cons (first input) acc))]
    [(equal? (first input) #\])
     (substring-brackets (rest input)
                         (rest brackets)
                         (cons (first input) acc))]
    [else
     (substring-brackets (rest input)
                         brackets
                         (cons (first input) acc))]))

(define (explode tree)
  "explodes the leftmost valid pair, returns the node or #f if none found"
  (let ([n (find-next-explosion tree)])
    (if (not n) #f
        (begin
          (set-left-of (first n) (rest n))
          (set-right-of (first n) (rest n))
          (zero-leaf (first n) (rest n))
          n))))

(define (find-next-split tree)
  "locates the next node that has a value >= 10"
  (or (and (node? (node-left tree)) 
           (find-next-split (node-left tree))) ; depth first look
      (and (number? (node-left tree))
           (>= (node-left tree) 10)
           tree)
      (and (node? (node-right tree))
           (find-next-split (node-right tree)))
      (and (number? (node-right tree))
           (>= (node-right tree) 10)
           tree)))

(define (split tree)
  (let ([n (find-next-split tree)])
    (if (not n) #f
        (cond
          [(and (not (node? (node-left n)))
                (>= (node-left n) 10))
           (set-node-left! n (node (floor (/ (node-left n) 2))
                                   (ceiling (/ (node-left n) 2))))
           n]
          [(and (not (node? (node-right n)))
                (>= (node-right n) 10))
           (set-node-right! n (node (floor (/ (node-right n) 2))
                                    (ceiling (/ (node-right n) 2))))
           n]))))

(define (cycle-es tree)
  (if (or (explode tree)
          (split tree))
      (cycle-es tree)
      tree))

(define (add-snail ft st)
  (node ft st))

(define (add-snail-list ft lst)
  (if (empty? lst) ft
      (add-snail-list (cycle-es (add-snail ft (first lst))) (rest lst))))

(define (magnitude n)
  (if (number? n) n
      (+ (* 3 (magnitude (node-left n)))
         (* 2 (magnitude (node-right n))))))

(define (largest-magnitude lst)
  "takes a list of strings and finds the largest magnitude combination amongst them"
  (define max-mag 0)
  (for ([l1 lst])
    (for ([l2 lst]
          #:when (not (equal? l1 l2)))
      (let ([m (magnitude
                (cycle-es (add-snail (parse-snail-string l1)
                                     (parse-snail-string l2))))])
        (when (> m max-mag)
          (set! max-mag m)))))
  max-mag)

(define (part-a)
  (define lines (file->lines "data/DayEighteen.txt"))
  (define tree (add-snail-list (parse-snail-string (first lines))
                               (map parse-snail-string (rest lines))))
  (magnitude tree))

(define (part-b)
  (define lines (file->lines "data/DayEighteen.txt"))
  (largest-magnitude lines))

(define (find-next-explosion tree [depth 0] [path (list)])
  "locates the next pair that is nested four deep, returns the path to it"
  (if (= depth 4) (cons tree path)
      (or (if (node? (node-left tree))
              (find-next-explosion (node-left tree) (+ depth 1) (cons tree path))
              #f)
          (if (node? (node-right tree))
              (find-next-explosion (node-right tree) (+ depth 1) (cons tree path))
              #f))))

(define (set-left-of n path [prev n])
  "adds the left value of n to the first node left of n"
  (cond
    [(empty? path) #f]
    [(eq? (first path) n) (set-left-of n (rest path) n)] ; need to at least reach parent
    [(eq? (node-left (first path)) prev)
     (set-left-of n (rest path) (first path))]
    ; we can access the left leaf and its a value
    [(and (eq? (node-right (first path)) prev)
          (not (node? (node-left (first path)))))
     (set-node-left! (first path) (+ (node-left (first path)) (node-left n)))
     (first path)]
    ; we can access the left leaf and its a node
    [(eq? (node-right (first path)) prev)
     (set-rightmost (node-left (first path)) (node-left n))]))

(define (set-right-of n path [prev n])
  "add the right value of n to the first node right of n"
  (cond
    [(empty? path) #f]
    [(eq? (first path) n) (set-right-of n (rest path) n)] ; need to at least reach parent
    [(eq? (node-right (first path)) prev)
     (set-right-of n (rest path) (first path))]
    ; we can access the right leaf and its a value
    [(and (eq? (node-left (first path)) prev)
          (not (node? (node-right (first path)))))
     (set-node-right! (first path) (+ (node-right (first path)) (node-right n)))
     (first path)]
    ; we can access the right leaf and its a node
    [(eq? (node-left (first path)) prev)
     (set-leftmost (node-right (first path)) (node-right n))]))

(define (zero-leaf n path)
  (cond
    [(eq? n (first path))
     (zero-leaf n (rest path))]
    [(eq? (node-left (first path)) n)
     (set-node-left! (first path) 0)
     (first path)]
    [(eq? (node-right (first path)) n)
     (set-node-right! (first path) 0)
     (first path)]))

(define (set-rightmost n v)
  (if (node? (node-right n))
      (set-rightmost (node-right n) v)
      (begin
        (set-node-right! n (+ (node-right n) v))
        n)))

(define (set-leftmost n v)
  (if (node? (node-left n))
      (set-leftmost (node-left n) v)
      (begin
        (set-node-left! n (+ (node-left n) v))
        n)))

(module+ test
  (require rackunit)

  (check-equal?
   (snail-num (string->list "12,"))
   (string->list "12"))

  (check-equal?
   (substring-brackets (string->list "[[1]], [2]"))
   (string->list "[[1]]"))

  (check-equal?
   (parse-snail (string->list "[1,2]"))
   (node 1 2))

  (struct basic-test (input expected))
  
  (define parsing-examples
    (list (basic-test "[1,2]" (node 1 2))
          (basic-test "[[1,2],3]" (node (node 1 2) 3))
          (basic-test "[9,[8,7]]" (node 9 (node 8 7)))
          (basic-test "[[1,9],[8,5]]" (node (node 1 9) (node 8 5)))
          (basic-test "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
                        (node (node (node (node 1 2)
                                          (node 3 4))
                                    (node (node 5 6)
                                          (node 7 8)))
                              9))
          (basic-test "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
                        (node (node (node 9
                                          (node 3 8))
                                    (node (node 0 9)
                                          6))
                              (node (node (node 3 7)
                                          (node 4 9))
                                    3)))
          (basic-test "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
                        (node (node (node (node 1 3)
                                          (node 5 3))
                                    (node (node 1 3)
                                          (node 8 7)))
                              (node (node (node 4 9)
                                          (node 6 9))
                                    (node (node 8 2)
                                          (node 7 3)))))))
  (for ([t parsing-examples])
    (check-equal?
     (parse-snail-string (basic-test-input t))
     (basic-test-expected t)))

  (check-equal?
   (first (find-next-explosion (parse-snail-string "[[[[[9,8],1],2],3],4]")))
   (node 9 8))

  (check-equal?
   (first (find-next-explosion (parse-snail-string "[7,[6,[5,[4,[3,2]]]]]")))
   (node 3 2))

  (check-equal?
   (first (find-next-explosion (parse-snail-string "[[6,[5,[4,[3,2]]]],1]")))
   (node 3 2))

  (let ([et (parse-snail-string "[[1,2],3]")])
    (check-equal?
     (set-left-of (node-left et) (list (node-left et) et))
     #f))
  (let ([et (parse-snail-string "[1,[2,3]]")])
    (check-equal?
     (set-left-of (node-right et) (list (node-right et) et))
     (node 3 (node 2 3))))
  (let ([et (parse-snail-string "[[1,2],3]")])
    (check-equal?
     (set-right-of (node-left et) (list (node-left et) et))
     (node (node 1 2) 5)))
  (let ([et (parse-snail-string "[1,[2,3]]")])
    (check-equal?
     (set-right-of (node-right et) (list (node-right et) et))
     #f))
  (let ([et (parse-snail-string "[[1,2],3]")])
    (zero-leaf (node-left et) (list (node-left et) et))
    (check-equal? et (node 0 3)))
  (let ([et (parse-snail-string "[1,[2,3]]")])
    (zero-leaf (node-right et) (list (node-right et) et))
    (check-equal? et (node 1 0)))


  (define explode-tests
    (list 
     (basic-test "[[[[[9,8],1],2],3],4]" "[[[[0,9],2],3],4]")
     (basic-test "[7,[6,[5,[4,[3,2]]]]]" "[7,[6,[5,[7,0]]]]")
     (basic-test "[[6,[5,[4,[3,2]]]],1]" "[[6,[5,[7,0]]],3]")
     (basic-test "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
     (basic-test "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")))

  (for ([t explode-tests])
    (let ([input (parse-snail-string (basic-test-input t))]
          [expected (parse-snail-string (basic-test-expected t))])
      (explode input)
      (check-equal? input expected)))

  (define split-tests
    (list
     (basic-test "[[[[0,7],4],[15,[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
     (basic-test "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")))

  (for ([t split-tests])
    (let ([input (parse-snail-string (basic-test-input t))]
          [expected (parse-snail-string (basic-test-expected t))])
      (split input)
      (check-equal? input expected)))

  (let ([input (parse-snail-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")]
        [expected (parse-snail-string "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")])
    (check-equal? (cycle-es input) expected))

  (check-equal? (add-snail (parse-snail-string "[[[[4,3],4],4],[7,[[8,4],9]]]")
                           (parse-snail-string "[1,1]"))
                (parse-snail-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))

  (check-equal? (add-snail-list (parse-snail-string "[[[[4,3],4],4],[7,[[8,4],9]]]")
                               (list (parse-snail-string "[1,1]")))
                (parse-snail-string "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

  (struct test-two (in-one in-two expected))

  (define add-tests
    (list (test-two "[1,1]"
                    (list "[2,2]" "[3,3]" "[4,4]")
                    "[[[[1,1],[2,2]],[3,3]],[4,4]]")
          (test-two "[1,1]"
                    (list "[2,2]" "[3,3]" "[4,4]" "[5,5]")
                    "[[[[3,0],[5,3]],[4,4]],[5,5]]")
          (test-two "[1,1]"
                    (list "[2,2]" "[3,3]" "[4,4]" "[5,5]" "[6,6]")
                    "[[[[5,0],[7,4]],[5,5]],[6,6]]")
          (test-two "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                    (list "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                          "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                          "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                          "[7,[5,[[3,8],[1,4]]]]"
                          "[[2,[2,2]],[8,[8,1]]]"
                          "[2,9]"
                          "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                          "[[[5,[7,4]],7],1]"
                          "[[[[4,2],2],6],[8,7]]")
                    "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")))

  (for ([t add-tests])
    (check-equal? (add-snail-list
                   (parse-snail-string (test-two-in-one t))
                   (map parse-snail-string (test-two-in-two t)))
                  (parse-snail-string (test-two-expected t))))

  (define mag-tests
    (list (basic-test "[[1,2],[[3,4],5]]" 143)
          (basic-test "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" 1384)
          (basic-test "[[[[1,1],[2,2]],[3,3]],[4,4]]" 445)
          (basic-test "[[[[3,0],[5,3]],[4,4]],[5,5]]" 791)
          (basic-test "[[[[5,0],[7,4]],[5,5]],[6,6]]" 1137)
          (basic-test "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" 3488)))
  (for ([t mag-tests])
    (check-equal? (magnitude (parse-snail-string (basic-test-input t)))
                  (basic-test-expected t)))

  (check-equal?
   (largest-magnitude
    (list "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
          "[[[5,[2,8]],4],[5,[[9,9],0]]]"
          "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
          "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
          "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
          "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
          "[[[[5,4],[7,7]],8],[[8,3],8]]"
          "[[9,3],[[9,9],[6,[4,9]]]]"
          "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
          "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"))
   3993)
  )
         
  
