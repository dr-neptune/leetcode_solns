#lang racket
(require racket)

;; idea
;; use a hash map
;; build up hash map with counts of each number
;; filter it to that which has a value of 1
;; return the key

(define exls '(4 1 2 1 2))

(define (build-hash ls hm)
  (if (empty? ls)
      hm
      (begin (if (hash-ref hm (first ls) #f)
                 (hash-update! hm (first ls) add1)
                 (hash-set! hm (first ls) 1))
             (build-hash (rest ls) hm))))

(define (filter-hash ht predicate)
  (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

(define (single-number nums)
  (let ([ht (make-hash)])
    (begin
      (build-hash nums ht)
      (first (hash-keys (filter-hash ht (lambda (v) (equal? v 1))))))))

;; trick solution bitwise xor
(define (single-number nums) (foldl bitwise-xor 0 nums))
