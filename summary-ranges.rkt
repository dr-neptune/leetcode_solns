#lang racket
(require racket)


#|

You are given a sorted unique integer array nums.

A range [a,b] is the set of all integers from a to b (inclusive).

Return the smallest sorted list of ranges that cover all the numbers
in the array exactly. That is, each element of nums is covered by
exactly one of the ranges, and there is no integer x such that x is in
one of the ranges but not in nums.

Each range [a,b] in the list should be output as:

    "a->b" if a != b
    "a" if a == b



Example 1:

Input: nums = [0,1,2,4,5,7]
Output: ["0->2","4->5","7"]
Explanation: The ranges are:
[0,2] --> "0->2"
[4,5] --> "4->5"
[7,7] --> "7"

Example 2:

Input: nums = [0,2,3,4,6,8,9]
Output: ["0","2->4","6","8->9"]
Explanation: The ranges are:
[0,0] --> "0"
[2,4] --> "2->4"
[6,6] --> "6"
[8,9] --> "8->9"

|#


#|

idea

we already have sorting, so we can partition into sets of runs
then take the first and last values in the sublist

|#

(define exls '(0 1 2 4 5 7))
(define exls '(0 2 3 4 6 8 9))

(map
 (λ (p) (cond [(= 1 (length p)) (number->string (first p))]
              [else (format "~a->~a" (first p) (second p))]))
 (for/fold ([prev (sub1 (apply min exls))]
           [agg '()]
           [results '()]
           #:result results)
          ([x (in-list exls)])
  (cond [(= x (add1 prev)) (if (= x (last exls))
                               (values x '() (cons (cons x agg) results))
                               (values x (cons x agg) results))]
        [else (if (= x (last exls))
                  (values x '() (cons `(,x) (cons agg results)))
                  (values x `(,x) (cons agg results)))])))



(define (summary-ranges nums)
  (map
   (λ (p) (cond [(= 1 (length p)) (number->string (first p))]
                [else (format "~a->~a" (first p) (second p))]))
   (for/fold ([prev (sub1 (apply min nums))]
              [agg '()]
              [results '()]
              #:result results)
             ([x (in-list nums)])
     (cond [(= x (add1 prev)) (if (= x (last nums))
                                  (values x '() (cons (cons x agg) results))
                                  (values x (cons x agg) results))]
           [else (if (= x (last nums))
                     (values x '() (cons `(,x) (cons agg results)))
                     (values x `(,x) (cons agg results)))]))))

;; currently also missing the 0! and the list is backwards
;; ["7","5->4","2->1"] should be ["0->2","4->5","7"]
