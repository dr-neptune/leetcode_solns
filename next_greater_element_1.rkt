#lang racket
(require racket)

;; given a number in a list
;; check another list for that number. Then get the next highest number in the rest of the list
;; if no such higher number exists, return -1

(define (check-rest val rsls)
  (let ([gt (filter (curryr > val) rsls)])
    (if (empty? gt) -1 (first gt))))

(define (check-val val ls)
  (let ([found (member val ls)])
    (check-rest val found)))

(define (next-greater-element nums1 nums2)
  (map (curryr check-val nums2) nums1))


(define exls1 '(4 1 2))
(define exls2 '(1 3 4 2))
(next-greater-element exls1 exls2)
