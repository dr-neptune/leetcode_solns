#lang racket
(require racket)

#|

Given an integer array nums and an integer k, return true if there are
two distinct indices i and j in the array such that nums[i] == nums[j]
and abs(i - j) <= k.


Example 1:

Input: nums = [1,2,3,1], k = 3
Output: true

Example 2:

Input: nums = [1,0,1,1], k = 1
Output: true

Example 3:

Input: nums = [1,2,3,1,2,3], k = 2
Output: false

|#

#|

idea

let x be a given number in the vector
make a counter where k is x, and v is (i x)
then filter the hash to those keys which have multiple values
then map a function that compares each element to see if there is a pair in which (i - j) < k
|#

(define exls '(1 2 3 1))
(define exk 3)

(define exls '(1 2 3 1 2 3))
(define exk 2)

(define (alist-hash ls)
  (let ([store (make-hash)])
    (for ([idx (in-range (length ls))]
          [i (in-list ls)])
      (if (hash-has-key? store i)
          (hash-set! store i (cons (cons idx i) (hash-ref store i)))
          (hash-set! store i (list (cons idx i)))))
    store))

(define (val-filter ht predicate)
  (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

(define (contains-nearby-duplicate nums k)
  (define (check-combinations indices)
    (define (close-indices? a b k)
      (<= (abs (- a b)) k))
    (for/or ([i (in-combinations indices 2)]
             #:final (close-indices? (first i) (second i) k))
      (close-indices? (first i) (second i) k)))
  (define get-indices (curry map car))
  (let ([store (alist-hash nums)])
    (ormap (compose check-combinations get-indices)
           (hash-values (val-filter store (compose (curry < 1) length))))))
