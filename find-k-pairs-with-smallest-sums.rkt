#lang racket
(require racket)

#|

idea

use combinations generator to add to a minheap
then pop the bottom 3

|#

(require data/heap)

(define (cons-sum<=? x y)
  (let ([cons-sum (λ (p) (+ (car p) (cdr p)))])
    (<= (cons-sum x)
        (cons-sum y))))



(define (sum<=? x y)
  (<= (apply + x)
      (apply + y)))

(let ([nums1 '(1 7 11)]
      [nums2 '(2 4 6)]
      [k 3]
      [hp (make-heap sum<=?)])
  (for* ([n1 nums1]
         [n2 nums2])
    (heap-add! hp (list n1 n2)))
  (for/list ([_ (in-range k)]
             [x (in-heap/consume! hp)])
    x))


(define (sum<=? x y)
  (<= (apply + x)
      (apply + y)))

(define (k-smallest-pairs nums1 nums2 k)
  (let ([hp (make-heap sum<=?)])
    (for* ([n1 nums1] [n2 nums2])
      (heap-add! hp (list n1 n2)))
    (for/list ([_ (in-range k)]
               [x (in-heap/consume! hp)])
      x)))

;; time limit exceeded
#|
idea

we need to stop the combinations at some particular point
maybe we can keep a max val and number of overwrites and once we hit k
we just return the heap consume

to start, let's just limit the nums to the first k items

|#

(define (sum<=? x y)
  (<= (apply + x)
      (apply + y)))

(define (k-smallest-pairs nums1 nums2 k)
  (let ([hp (make-heap sum<=?)])
    (for* ([n1 (take nums1 k)] [n2 (take nums2 k)])
      (heap-add! hp (list n1 n2)))
    (for/list ([_ (in-range k)]
               [x (in-heap/consume! hp)])
      x)))

#|

we know that they are increasing

maybe do it without the min-heap

just take values while they are increasing and stop at k

|#

(let ([nums1 '(1 7 11)]
      [nums2 '(2 4 6)]
      [k 3])
  (for*/fold ([pair-sums '()]
              [k k]
              #:result (reverse pair-sums))
             ([n1 nums1]
              [n2 nums2]
              #:break (zero? k))
    (cond [(empty? pair-sums)
           (values (cons (list n1 n2) pair-sums) (sub1 k))]
          [(>= (+ n1 n2) (apply + (first pair-sums)))
           (values (cons (list n1 n2) pair-sums) (sub1 k))])))


;; maybe zip combinations?

(require (only-in srfi/1 zip))


(let ([nums1 '(1 7 11)]
      [nums2 '(2 4 6)]
      [k 3])
  (for*/fold ([pair-sums '()]
              [k k]
              #:result (reverse pair-sums))
             ([n1 nums1]
              [n2 nums2]
              #:break (zero? k))
    (cond [(empty? pair-sums)
           (values (cons (list n1 n2) pair-sums) (sub1 k))]
          [(>= (+ n1 n2) (apply + (first pair-sums)))
           (values (cons (list n1 n2) pair-sums) (sub1 k))])))



(let loop ([nums1 '(1 7 11)]
           [nums2 '(2 4 6)]
           [k 3]
           [pair-sums '()])
  (cond [(zero? k) (reverse pair-sums)]
        [(empty? pair-sums)
         (loop nums2 (rest nums1) (sub1 k) (cons (list (first n1) (first n2)) pair-sums))]
        [(>= (+ (first n1) (first n2)) (apply + (first pair-sums)))
         (loop nums)]))


#|

alternating iterator

[nums1 '(1 7 11)]
[nums2 '(2 4 6)]

-> (1 2), (2 1), (1 4), (2, 7), (7, 4), (4, 7), (7 6), (4 11), ...

-> (1 4) (2 7) (1 2) (7 6) (4 11) (7 4) (11 ()) (6 ()) (11 6)

|#
(define (alternating-stream lst1 lst2)
  (define (iter l1 l2)
    (cond
      [(and (null? l1) (null? l2)) empty-stream]
      [(null? l1) (stream-cons (list (first l2) (first l1)) (iter l1 (rest l2)))]
      [(null? l2) (stream-cons (list (first l1) (first l2)) (iter (rest l1) l2))]
      [else (stream-append
              (stream-cons (list (first l1) (first l2)) empty-stream)
              (stream-cons (list (first l2) (if (null? (rest l1)) '() (second l1))) empty-stream)
              (stream-cons (list (first l1) (if (null? (rest l2)) '() (second l2))) empty-stream)
              (iter (rest l1) (rest l2)))]))
  (iter lst1 lst2))

; Example usage
(define my-stream (alternating-stream '(1 7 11) '(2 4 6)))
(stream-ref my-stream 0) ; Get the first element of the stream

(for/list ([i (in-stream my-stream)])
  i)


(let* ([nums1 '(1 7 11)]
       [nums2 '(2 4 6)]
       [k 3])
  (for*/fold ([pair-sums '()]
              [k k]
              #:result (reverse pair-sums))
             ([pair (in-stream (alternating-stream nums1 nums2))]
              #:break (zero? k))
    (displayln (format "~a ~a" pair k))
    (cond [(empty? pair-sums)
           (values (cons pair pair-sums) (sub1 k))]
          [(>= (apply + pair) (apply + (first pair-sums)))
           (values (cons pair pair-sums) (sub1 k))])))


;; go back to original plan
(define (sum<=? x y)
  (<= (apply + x)
      (apply + y)))

(let ([nums1 '(1 7 11)]
      [nums2 '(2 4 6)]
      [k 3]
      [hp (make-heap sum<=?)])
  (for* ([n1 nums1]
         [n2 nums2])
    (heap-add! hp (list n1 n2)))
  (for/list ([_ (in-range k)]
             [x (in-heap/consume! hp)])
    x))
