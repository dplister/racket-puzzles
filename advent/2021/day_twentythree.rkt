#lang racket

(struct location (tenant position))

(define start-positions
  (list #\.
        #\.
        (list #\B #\A #\A)
        #\.
        (list #\C #\D #\B)
        #\.
        (list #\B #\C #\C)
        #\.
        (list #\D #\A #\D)
        #\.
        #\.))

(define (valid-moves state)
  "generates set of possible valid moves"
  (define candidates (filter (lambda (p) (can-move? p state)) state)))

(define (can-move? p state)
  (cond
    [(list? p)
     ; check if all items are meant to be in that room
     #f
     ; if not, then yes the list can be emptied
     #t]
    [else
     ; if its a singular item, its in the hallway; check the room its meant to go to only has valid items in it
     #t]))
              
(define (valid-moves loc state)
  ; we assume if p is flagged for movement it isn't where it is meant to be
  ; if it is in a room, can it move to its correct room (i.e. does destination only have those same type?)
  (cond
    [(list? (location-tenants loc))
     (let ([fm (first (location-tenants loc))])

  ; otherwise, what reachable spots are available?)

  (define (find-matching-room p state)
    (first (filter (lambda (s) (and (list? s) (equal? (last s) p))) state)))

  (define (room-contents-identical? rm)
    (empty? (filter (lambda (p) (not (equal? p (last rm)))) rm)))
  
  (define (can-reach? from to state)
    (define start (min from to))
    (define end (max from to))
    (define path (take (drop state start) (- end start)))
    (define blockers (filter (lambda (p) (not (or (equal? p #\.)
                                                  (list? p)))))
      path)
    (let ([rto (list-ref state to)])
      (and (empty? blockers)
           ; is a room with a spot
           (or (and (list? rto) (< (length rto) 3))
               ;
               (not (list? rto))))))
      
