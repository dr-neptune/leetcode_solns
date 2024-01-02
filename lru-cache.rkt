#lang racket
(require racket)

#|

idea

keep a running tally of the values going in with a cons list
keep the values in a mutable hash

|#

(define lru-cache%
  (class object%
    (super-new)

    ; capacity : exact-integer?
    (init-field
     capacity
     [vals (make-hash)]
     [seen '()])

    (define/private (update-seen! val)
      (set! seen (cons val (remove val seen)))
      (when (> (length seen) capacity)
        (let ([last-val (last seen)])
          (hash-remove! vals last-val)
          (set! seen (drop-right seen 1)))))

    ; get : exact-integer? -> exact-integer?
    (define/public (get key)
      (let ([ret-val (hash-ref vals key -1)])
        (when ((compose not negative?) ret-val)
          (update-seen! key))
        ret-val))

    ; put : exact-integer? exact-integer? -> void?
    (define/public (put key value)
      (hash-set! vals key value)
      (update-seen! key))))


;; time limit exceeded
#|

idea

use just a hash?

|#

(define lru-cache%
  (class object%
    (super-new)

    ; capacity : exact-integer?
    set
    (init-field
     capacity
     [vals (make-hash)])

    ; get : exact-integer? -> exact-integer?
    (define/public (get key)
      (let ([ret-val (hash-ref vals key -1)])
        (when ((compose not negative?) ret-val)
          (hash-remove! vals key)
          (hash-set! vals key ret-val))
        (begin
          (displayln (format "~a" vals))
          ret-val)))

    ; put : exact-integer? exact-integer? -> void?
    (define/public (put key value)
      (if (hash-has-key? vals key)
          (begin
            (hash-remove! vals key)
            (hash-set! vals key value))
          (hash-set! vals key value))

      (let ([hash-size (hash-count vals)])
        (when (> hash-size capacity)
          (displayln (format "vals keys: ~a" (hash-keys vals)))
          (hash-remove! vals (first (hash-keys vals)))))
      (displayln (format "~a" vals)))))



(define lru-cache%
  (class object%
    (super-new)

    ; capacity : exact-integer?
    set
    (init-field
     capacity
     [vals (make-hash)])

    ; get : exact-integer? -> exact-integer?
    (define/public (get key)
      (let ([ret-val (hash-ref vals key -1)])
        (when ((compose not negative?) ret-val)
          (hash-remove! vals key)
          (hash-set! vals key ret-val))
        ret-val))

    ; put : exact-integer? exact-integer? -> void?
    (define/public (put key value)
      (if (hash-has-key? vals key)
          (begin
            (hash-remove! vals key)
            (hash-set! vals key value))
          (hash-set! vals key value))

      (let ([hash-size (hash-count vals)])
        (when (> hash-size capacity)
          (hash-remove! vals (first (hash-keys vals))))))))

(define obj (new lru-cache% [capacity capacity]))

(send obj put 1 1)
(send obj put 2 2)
(send obj get 1)
(send obj put 3 3)
(send obj get 2)
(send obj put 4 4)
(send obj get 1)
(send obj get 3)
(send obj get 4)


(send obj put 2 1)
(send obj put 2 2)  ;; here
(send obj get 2)
(send obj put 1 1)
(send obj put 4 1)
(send obj get 2)


;; (define param_1 (send obj get key))
;; (send obj put key value)

(send obj put 1 1)
(send obj put 2 2)
(send obj get 1)
(send obj put 3 3)
(send obj get 2)
(send obj put 4 4)
(send obj get 1)
(send obj get 3)
(send obj get 4)

;; (send obj put 1 0)
;; (send obj put 2 2)
;; (send obj get 1)
;; (send obj put 3 3)
;; (send obj get 2)  ;; here
;; (send obj put 4 4)
;; (send obj get 1)
;; (send obj get 3)
;; (send obj get 4)


;; try it again with editorial help
(struct list-node
  (key val prev next) #:mutable #:transparent)

; constructor
(define (make-list-node key val [prev #f] [next #f])
  (list-node key val prev next))


(define lru-cache%
  (class object%
    (super-new)

    ; capacity : exact-integer?
    (init-field
     capacity
     [hsh (make-hash)]
     [head (make-list-node -1 -1)]
     [tail (make-list-node -1 -1)])

    (set-list-node-next! head tail)
    (set-list-node-prev! tail head)

    (define/private (add node)
      (let ([previous-end (list-node-prev tail)])
        (set-list-node-next! previous-end node)
        (set-list-node-prev! node previous-end)
        (set-list-node-next! node tail)
        (set-list-node-prev! tail node)))

    ;; (define/private (remove node)
    ;;   (set-list-node-next! (list-node-prev node (list-node-next node))))

    ; get : exact-integer? -> exact-integer?
    (define/public (get key)
      (if (not (hash-has-key? hsh key))
          -1
          (let ([node (hash-ref dic key)])
            (add node))))

    ; put : exact-integer? exact-integer? -> void?
    (define/public (put key value)
      (list-node-prev tail))))



;; this shit sucks
#|

try it with a dict and a list
|#

(require data/gvector)

(define (gvector-remove-element gv ele)
  (for/gvector ([g gv]
                #:when (not (equal? g ele)))
    g))

(define lru-cache%
  (class object%
    (super-new)

    ; capacity : exact-integer?
    (init-field
     capacity
     [vals (make-hash)]
     [seen (make-gvector #:capacity capacity)])

    (define/private (update-seen! val)
      ;;(set! seen (gvector-remove-element seen val))
      (gvector-insert! seen 0 val)
      (let ([gvec-len (gvector-count seen)])
      (when (> gvec-len capacity)
        (let ([last-val (gvector-ref seen (sub1 gvec-len))])
          (hash-remove! vals last-val)
          (begin
            (gvector-remove-last! seen)
            (void))))))

    ; get : exact-integer? -> exact-integer?
    (define/public (get key)
      (let ([ret-val (hash-ref vals key -1)])
        (when ((compose not negative?) ret-val)
          (update-seen! key))
        ret-val))

    ; put : exact-integer? exact-integer? -> void?
    (define/public (put key value)
      (hash-set! vals key value)
      (update-seen! key))))

(define obj (new lru-cache% [capacity capacity]))

(send obj put 1 1)
(send obj put 2 2)
(send obj get 1)
(send obj put 3 3)
(send obj get 2)
(send obj put 4 4)
(send obj get 1)
(send obj get 3)
(send obj get 4)
