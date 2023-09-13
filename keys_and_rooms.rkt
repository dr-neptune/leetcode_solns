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
