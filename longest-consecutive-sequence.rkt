#lang racket
(require racket)

#|

Example 1:

Input: nums = [100,4,200,1,3,2]
Output: 4
Explanation: The longest consecutive elements sequence is [1, 2, 3, 4]. Therefore its length is 4.

Example 2:

Input: nums = [0,3,7,2,5,8,4,6,0,1]
Output: 9

|#

;; idea
;; maybe we can add all the elements to a min-heap
;; then we can iterate through the min-heap and count up the maximum streak

(define exls '(0 3 7 2 5 8 4 6 0 1))

(define exls '(100 4 200 1 3 2))

(define exls '(1 2 0 1))

(require data/heap)

(let ([hp (make-heap <=)])
  (heap-add-all! hp exls)
  (define min-val (apply min exls))
  (for/fold ([count 1]
             [prev min-val]
             [max-val 1]
             #:result max-val)
            ([x (in-heap/consume! hp)])
    (if (= x (add1 prev))
        (values (add1 count) x (max (add1 count) max-val))
        (values 1 x max-val))))


(require data/heap)

(define (longest-consecutive nums)
  (match nums
    ['() 0]
    [_ (let ([hp (make-heap <=)])
         (heap-add-all! hp nums)
         (define min-val (apply min nums))
         (for/fold ([count 0]
                    [prev min-val]
                    [max-val min-val]
                    #:result max-val)
                   ([x (in-heap/consume! hp)])
           (if (= x (add1 prev))
               (values (add1 count) x (max (add1 count) max-val))
               (values 1 x max-val))))]))


(require data/heap)

(define (longest-consecutive nums)
  (match nums
    ['() 0]
    [(list a) 1]
    [_ (let ([hp (make-heap <=)])
         (heap-add-all! hp nums)
         (define min-val (apply min nums))
         (for/fold ([count 0]
                    [prev min-val]
                    [max-val 1]
                    #:result max-val)
                   ([x (in-heap/consume! hp)])
           (if (= x (add1 prev))
               (values (add1 count) x (max (add1 count) max-val))
               (values 1 x max-val))))]))
