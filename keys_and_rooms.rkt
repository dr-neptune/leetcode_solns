#lang racket
(require racket)

(define exgraph '((1) (2) (3) ()))

;; idea
;; create a hash table containing rooms 0 -> n-1 set to #f
;; go to room 0 and set room 0 ht to #t
;; each item in room 1 determines our next steps

;; start out by just printing which keys are in each room
(define rms (list->vector exgraph))

(let ([rooms exgraph])
  (let loop ([rms (list->vector rooms)]
             [i (in-range (length rooms))])
    (if (zero? (vector-length rms))
        #t
        (let ([room (vector-ref rms i)])
          (for/list ([key room])
            (println (vector-ref rms key)))))))

(let ([rooms exgraph])
  (for/list ([room rooms])
    (for ([key room])
      (println key))))

(let loop ([rooms exgraph])
  (if (empty? rooms)
      '()
      (let ([room (first rooms)])
        (for/list ([key room])
          (loop (list-ref rooms key))))))

(define exgraph2 '((1 3) (3 0 1) (2) (0)))

(let ([rooms exgraph2])
  (for/list ([room rooms])
    (for/list ([key room])
      (displayln (format "key: ~a room: ~a" key room))
      (list-ref rooms key))))

;; get keys in first room
;; then get the keys in each of the rooms we have keys to in the first room
;; and so on, until we have found all of the number keys 0->n-1

(let ([rooms exgraph2])
  (let loop ([keys (first rooms)])
    (if (empty? keys)
        '()
        (for/list ([key keys])
          (list key (list-ref rooms key))))))

(list-ref exgraph2 0)

;; we should append rooms that we can access to a list
;; then at the end, we can sort both and check if equal

(let ([rooms exgraph2])
  (let loop ([room (first rooms)]
             [rm-idx 0]
             [new-rooms '()])
    ;; (displayln (format "room: ~a new-rooms: ~a" room new-rooms))
    (if (empty? room)
        new-rooms
        (for/list ([key room])
          ;; (displayln (format "using key: ~a" key))
          (let ([new-room (list-ref rooms key)])
            (if (not (member key new-rooms))
                (loop new-room
                      key
                      (cons rm-idx new-rooms))
                #f))))))


;; we need a way to account for loops
;; to be brute, we can start with removing values from a list

(let ([rooms exgraph2])
  (let loop ([room (first rooms)]
             [room-idx 0]
             [rooms-seen '(0)])
    (displayln (format "~a ~a" room-idx rooms-seen))
    (for ([key room]
          #:when (not (member key rooms-seen)))
      (if (empty? room)
          '()
          (loop (list-ref rooms key)
                key
                (cons key rooms-seen))))))

;; what if I can fold it?
(let ([rooms exgraph2])
  (let loop ([room (first rooms)])
    (for/fold ([rooms-seen '(0)])
              ([key room])
      ())))


(let ([rooms exgraph2])
  (for/fold ([rooms-seen '(0)])
            ([keys '(1 3)])
    (displayln (format "outer: ~a ~a" keys rooms-seen))
    (for/list ([key keys])
      (displayln (format "inner: ~a" key))
      (values (list-ref rooms key)))))

;; okididdlyokily

;; idea
;; visit each room
;; when you get new keys, add them to a set
;; for each key that is not in the set already, view the room and add it to the set
(let* ([rooms exgraph2]
       [num-rooms (length rooms)])
  (let dfs ([room (first rooms)]
            [seen-keys (list->set (cons 0 (first rooms)))])
    (for/set ([key room])
      (if (not (set-member? seen-keys key))
          key
          (set-union (list->set (list-ref rooms key)) seen-keys)))))

(let* ([rooms exgraph2]
       [num-rooms (length rooms)]
       [room (first rooms)]
       [seen-keys (list->set (cons 0 (first rooms)))])
  (for/set ([key room])
      (if (not (set-member? seen-keys key))
          key
          (set-union (list->set (list-ref rooms key)) seen-keys))))

;; now we need to see if we have all the rooms
(define (can-visit-all-rooms rooms)
  (let* ([num-rooms (length rooms)]
         [room (first rooms)]
         [seen-keys (list->set (first rooms))])
    (identity ;eq? num-rooms
         (identity ;(compose set-count set-first)
          (for/set ([key room])
            (displayln (format "~a ~a" key seen-keys))
            (if (not (set-member? seen-keys key))
                key
                (set-union (list->set (list-ref rooms key)) seen-keys)))))))

(can-visit-all-rooms exgraph2)
(can-visit-all-rooms exgraph)

(define exgraph3 '((1) (2) () (3)))

;; how 2 br00t force?
;; start with first room
;; then add keys
(define rooms exgraph2)

(let ([rooms exgraph])
  (let loop ([room-key (first rooms)]
             [held-keys (list->set (first rooms))])
    (if (null? room-key)
        held-keys
        (loop (rest room-key)
              (set-union (list->set (list-ref rooms (first room-key))) held-keys)))))

;; we need to recurse!

;; grab the keys from the first room
;; for each key in the first room
;;   unlock the door of the room for the key and recurse with those keys
;;     but filter out keys that are already seen
;;   if a room is empty after filtering then return the list of rooms seen

(let ([rooms exgraph2])
  (let ([room (first rooms)])
    (let loop ([keys (first rooms)])
      (displayln (format "keys: ~a" keys))
      (if (empty? keys)
          '()
          (cons (first keys)
                (for/list ([key keys])
                  (let ([new-room (filter (λ (v) (not (member v keys)))
                                          (list-ref rooms key))])
                    (loop new-room))))))))


(define (dfs rooms idx rooms-visited)
  (map (λ (j)
         (if (not (vector-ref rooms-visited j))
             (begin
               (vector-set! rooms-visited j #t)
               (dfs rooms j rooms-visited))
             #f))
       (list-ref rooms idx)))



(define (can-visit-all-rooms rooms)
  (let ([rooms-visited (make-vector (length rooms) #f)])
    (vector-set! rooms-visited 0 #t)
    (dfs rooms 0 rooms-visited)
    (andmap identity (vector->list rooms-visited))))

(can-visit-all-rooms exgraph)
(can-visit-all-rooms exgraph2)

(define (dfs rooms visited)
  (map (λ (rm)
         (map (λ (key)
                (if (member key visited)
                    #f
                    (dfs (list-ref rooms key) (cons key visited))))
              (if (list? rm)
                  rm
                  (list rm))))
       rooms))

(dfs exgraph2 '())


;; inner
;; for a given list of keys
;; if we have seen it, then pass
;; if we haven't seen it, then inner it

;; outer
;; for each of them, do inner


#|

for each room
    for each key in room
        if room has been seen, make a tally and then recurse
 else return '()

|#

(let ([rooms exgraph2]
      [seen-keys '()])
  (for/list ([room rooms])
    (let loop ([key room])
      (if (empty? key)
          '()
          (cons (vector-member))))))

(let ([rooms exgraph2])
  (let loop ([room (list->vector rooms)]
             [seen-rooms (vector-append #(#t) (make-vector #f (length rooms)))])
    (if (vector-ref seen-rooms)
        #()
        (begin
          (vector-set! seen-rooms )))))

(let ([rooms exgraph2])
  (let ([seen (vector-append #(#t) (make-vector (sub1 (length rooms)) #f))])
    (for/vector ([room rooms])
      (let loop ([]))
      (for/vector ([key room])
        (if (not (vector-ref seen key))
            (vector-set! seen key #t)
            #f)))
    seen))


;; again!

;; idea
;; keep a constant time lookup data structure that has whether a room has been visited
;; grab keys from 0th room
;; for each key, grab keys from that room
;; if a key has been seen, then change the ds
;; otherwise go to the key room

(let ([rooms exgraph2])
  (let outer ([keys (first rooms)]
              [seen-rooms (make-hash)])  ;; need to add 0th room
    (let inner ([key keys])
      (if (empty? keys)
          #f
          (if (hash-has-key? seen-rooms (first key))
              (inner (rest keys))
              (begin
                (hash-update! seen-rooms (first key) add1 0)
                (outer (list-ref rooms (first key)) seen-rooms)))))))


(let ([rooms exgraph3]
      [seen-rooms (make-hash '((0 1)))])
  (define (dfs room)
    (let loop ([keys room])
      (if (empty? keys)
          '()
          (if (hash-has-key? seen-rooms (first keys))
              '()
              (begin
                (hash-update! seen-rooms (first keys) add1 0)
                (dfs (list-ref rooms (first keys))))))))
  (dfs (first rooms))
  (eq? (length (hash-keys seen-rooms))
       (length rooms)))

(define (can-visit-all-rooms rooms)
  (let ([seen-rooms (make-hash '((0 1)))])
    (define (dfs room)
      (let loop ([keys room])
        (if (empty? keys)
            '()
            (if (hash-has-key? seen-rooms (first keys))
                (loop (rest keys))
                (begin
                  (hash-update! seen-rooms (first keys) add1 0)
                  (dfs (list-ref rooms (first keys))))))))
    (dfs (first rooms))
    (eq? (length (hash-keys seen-rooms))
         (length rooms))))


;; almost! 55/68
(define exgraph4 '((1 3 2) (2 3) (2 3 1) ()))

(let ([rooms exgraph5]
      [seen-rooms (make-hash '((0 1)))])
  (define (dfs room)
    (let loop ([keys room])
      (displayln (format "seen: ~a keys: ~a" seen-rooms keys))
      (if (empty? keys)
          seen-rooms
          (if (hash-has-key? seen-rooms (first keys))
              (loop (rest keys))
              (begin
                (hash-update! seen-rooms (first keys) add1 0)
                (displayln (format "going to room: ~a" (first keys)))
                (dfs (list-ref rooms (first keys))))))))
  (dfs (first rooms))
  (eq? (length (hash-keys seen-rooms))
       (length rooms)))

;; idea
;; for each key, just update seen-rooms
(define exgraph5 '((2 3) () (2) (1 3)))


(let ([rooms exgraph5]
      [seen-rooms (make-hash '((0 1)))])
  (define (dfs room)
    ;; (displayln (format "seen: ~a room: ~a" seen-rooms room))
    (map (λ (key)
           (if (hash-has-key? seen-rooms key)
               #f
               (begin
                 (hash-update! seen-rooms key add1 0)
                 (identity ;;begin
                   ;;(displayln (format "going to room ~a" key))
                  (dfs (list-ref rooms key))))))
         room))
  (begin
    (dfs (first rooms))
    (equal? (length (hash-keys seen-rooms))
            (length rooms))))

(define (can-visit-all-rooms rooms)
  (let ([seen-rooms (make-hash '((0 1)))])
    (define (dfs room)
      (map (λ (key)
             (if (hash-has-key? seen-rooms key)
                 #f
                 (begin
                   (hash-update! seen-rooms key add1 0)
                   (dfs (list-ref rooms key)))))
           room))
    (begin
      (dfs (first rooms))
      (equal? (length (hash-keys seen-rooms))
              (length rooms)))))


(define (can-visit-all-rooms rooms)
  (let ([seen-rooms (make-hash '((0 1)))])
    (define (dfs room)
      (map (λ (key)
             (if (hash-has-key? seen-rooms key)
                 #f
                 (begin
                   (hash-update! seen-rooms key add1 0)
                   (dfs (list-ref rooms key)))))
           room))
    (begin
      (dfs (first rooms))
      (= (hash-count seen-rooms)
         (length rooms)))))
