#lang racket
(require racket)

;; idea
;; make a counter dict for s2
;; then traverse s1, removing a count for each char
;; return whatever one is not 0

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (val-filter ht predicate)
  (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

(define (find-the-difference s t)
  (let ([ht (hash-table-counter (string->list t))])
    (for-each (λ (c) (hash-update! ht c sub1)) (string->list s))
    (first (hash-keys (val-filter ht (λ (v) (not (zero? v))))))))


(define exstr1 "abcd")
(define exstr2 "abcde")
(find-the-difference exstr1 exstr2)

(find-the-difference "" "y")


;; try again
;; idea
;; traverse both strings at the same time
;; if char s != char t, return char t
(define (find-the-difference s t)
  (let ([sls (sort (string->list s) char<?)]
        [tls (sort (string->list t) char<?)])
    (let loop ([s sls]
               [t tls])
      (match (list s t)
        [(list '() a) (first a)]
        [(list (list a _ ...) (list a _ ...)) (loop (rest s) (rest t))]
        [_ (first t)]))))

(find-the-difference "" "t")
(find-the-difference "abcd" "abcde")