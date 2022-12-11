#lang racket

(struct point (x y))

(define (move 
	  [head (list (point 0 0))] 
	  [tail (list (point 0 0))] 
	  moves)
  (cond
    [(empty? moves) (values head tail)]
    [else
      (define hp (point (+ (point-x head) (point-x (first move)))
			(+ (point-y head) (point-y (first move)))))
      (define tp (follow hp (first tail)))
      (move
	(cons hp head)
	(cons tp tail)
	(rest moves))]))
    
(define (follow head tail)
  
