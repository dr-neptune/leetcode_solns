#lang racket
(require racket)

#|
Given an array of integers citations where citations[i] is the number
of citations a researcher received for their ith paper, return the
researcher's h-index.

According to the definition of h-index on Wikipedia: The h-index is
defined as the maximum value of h such that the given researcher has
published at least h papers that have each been cited at least h
times.


Example 1:

Input: citations = [3,0,6,1,5]
Output: 3
Explanation: [3,0,6,1,5] means the researcher has 5 papers in total and each of them had received 3, 0, 6, 1, 5 citations respectively.
Since the researcher has 3 papers with at least 3 citations each and the remaining two with no more than 3 citations each, their h-index is 3.

Example 2:

Input: citations = [1,3,1]
Output: 1

|#

(define exls '(3 0 6 1 5))
(define exls2 '(1 3 1))


#|
idea

sort papers
binary search to find maximum value that words according to pred?
this is nlogn + logn + n

could be even easier
if we sort the list, then everything after the idx has at least val citations
then each val before the idx has less
so the max would be either n//2 if even num of elements, or n//2 + 1

h-index = max(cit[i] >= i)
|#

(define exls4 '(0))

(define (enumerate ls)
  (map cons (stream->list (in-inclusive-range 1 (length ls))) ls))


(let ([citations exls4])
  (let* ([scit (sort citations >)]
         [ind-scit (enumerate scit)])
    (filter (λ (pair) (>= (cdr pair) (car pair))) ind-scit)))


(define (enumerate ls)
  (map cons (stream->list (in-inclusive-range 1 (length ls))) ls))

(define (h-index citations)
  (let* ([scit (sort citations >)]
         [ind-scit (enumerate scit)])
    (car (last (filter (λ (pair) (>= (cdr pair) (car pair))) ind-scit)))))


(let ([citations exls2])
  (let* ([scit (sort citations <)]
         [cit-len (length citations)]
         [val (if (even? cit-len)
                  (list-ref scit (sub1 (quotient cit-len 2)))
                  (list-ref scit (quotient cit-len 2)))])
    val))

(define (h-index citations)
  (let* ([scit (sort citations <)]
         [cit-len (length citations)]
         [val (if (even? cit-len)
                  (list-ref scit (sub1 (quotient cit-len 2)))
                  (list-ref scit (quotient cit-len 2)))])
    val))
