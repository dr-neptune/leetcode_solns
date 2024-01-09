#lang racket
(require racket)

#|

idea

|#

(define exls '(1 2 3))

(define exls '(1 1 2 2))

(define (val-filter ht predicate)
  (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

(let* ([hsh (make-hash)])
  (begin
    (for ([i exls]) (hash-set! hsh i 0))
    (for ([(k v) (in-hash hsh)])
      (displayln (format "k: ~a v: ~a hsh: ~a" k v hsh))
      (when (hash-ref hsh (sub1 k) #f)
        (hash-update! hsh (sub1 k) add1)))
    (val-filter hsh (λ (v) (not (zero? v))))))



(define (count-elements arr)
  (define (val-filter ht predicate)
    (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

  (let* ([hsh (make-hash)])
    (begin
      (for ([i arr]) (hash-set! hsh i 0))
      (for ([(k v) (in-hash hsh)])
        (when (hash-ref hsh (sub1 k) #f)
          (hash-set! hsh (sub1 k) 1)))
      (val-filter hsh (λ (v) (< 0 v))))))
