#lang racket

(require threading)

(define (parse-line input)
  (match input
    [(regexp #rx"^\\$ cd (.+)" (list _ dir))
     (list 'move dir)]
    [(regexp #rx"^\\$ ls" (list _))
     (list 'ls)]
    [(regexp #rx"^dir (.+)" (list _ dirname))
     (list 'dir dirname)]
    [(regexp #rx"^([0-9]+) (.+)" (list _ size filename))
     (list 'file filename (string->number size))]))

(define (add-item ds path item)
  (hash-update ds path 
	       (lambda (v) 
		 (cond 
		   [(empty? item) v]
		   [(list? (first item)) (append item v)]
		   [else (cons item v)]))
	       '()))

(define (execute moves [path (list "/")] [ds (hash)])
  (if (empty? moves) ds
    (match (first moves)
      [(list 'move "/")
       (execute (rest moves)
		(list (last path))
		ds)]
      [(list 'move dir)
       (execute (rest moves) 
		(cons dir path)
		(~> ds
		    ; add to existing path if we blind visited
		    (add-item (first path) (list 'dir dir))
		    ; create path if first visit
		    (add-item dir (list))))]
      [(list 'ls)
       (execute (rest moves) path ds)]
      [else
       (execute (rest moves)
		path
		(add-item ds (first path) (first moves)))])))

(define (calculate-totals ds)
  "searches the set of directories and calculates their total sizes"
  ; we could store calculations as we go for efficiency
  (define (total dir)
    (apply + (foldl + 0 (filter-map (lambda (f) (and (equal? 'file (first f))
						     (last f)))
				    (hash-ref ds dir)))
	   (map total (filter-map (lambda (d) (and (equal? 'dir (first d))
						   (last d)))
				  (hash-ref ds dir)))))
  (map (lambda (k) (list k (total k))) (hash-keys ds)))

(module+ test
  (require rackunit)

  (check-equal? (parse-line "$ cd /") (list 'move "/"))
  (check-equal? (parse-line "$ ls") (list 'ls))
  (check-equal? (parse-line "dir d") (list 'dir "d"))
  (check-equal? (parse-line "62596 h.lst") (list 'file "h.lst" 62596))

  (check-equal? (add-item (hash) "/" '(dir "asdf")) (hash "/" '((dir "asdf"))))
  (check-equal? (add-item
		  (add-item (hash) "/" '(dir "asdf")) 
		  "/" '(dir "fdsa"))
		(hash "/" '((dir "fdsa") (dir "asdf"))))

  (check-equal? (execute '((move "a"))) (hash "/" '((dir "a")) "a" '()))

  (define example (file->lines "day_seven.example"))

  (check-equal? 
    (calculate-totals
      (execute 
	(map parse-line example)))
    '(("e" 584) ("a" 94853) ("d" 24933642) ("/" 48381164)))
  

)
