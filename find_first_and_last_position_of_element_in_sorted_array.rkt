#lang racket
(require racket)

(define exls '(5 7 7 8 8 10))

;; straight-forward solution
(define (search-range nums target)
  (let ([idx (indexes-of nums target)])
    (if (empty? idx)
        '(-1 -1)
        (list (apply min idx)
              (apply max idx)))))

(search-range exls 8)
(search-range exls 6)
(search-range '() 0)

;; with binary search

;; naive approach
;; find any instance of our target, and then do a bidirectional scan

;; a better approach is to use 2 binary searches, one for each cap
;; normally we compare nums[mid] == target, but now, apart from checking
;; for equality, we also need to check if mid is the first or the last
;; index where the target occurs.

;; first position in array
;; 2 instances:
;; mid == begin -> mid element is first element in remaining subarray
;; the element to the left of the index is not equal to the target
;; i.e. nums[mid - 1] != target

;; last position in array
;; 2 instances:
;; if mid == end -> mid element is the last element in remaining subarray
;; if element to the right of mid is != target i.e. nums[mid - 1] != target

;; now make it use vectors
(define (find-bound vec target first-val)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (vector-ref vec mid)])
      (cond [(> left right) -1]
            [(= target mid-val) (if first-val
                                    (if (or (= mid left)
                                            (< (vector-ref vec (sub1 mid)) target))
                                        mid
                                        (ptr-narrow left (sub1 right)))
                                    (if (or (= mid right)
                                            (> (vector-ref vec (add1 mid)) target))
                                        mid
                                        (ptr-narrow (add1 left) right)))]
            [(> mid-val target) (ptr-narrow left (sub1 mid))]
            [else (ptr-narrow (add1 mid) right)])))
  (ptr-narrow 0 (sub1 (vector-length vec))))


(define (search-range nums target)
  (if (empty? nums)
      '(-1 -1)
      (let* ([nums (list->vector nums)]
             [fst-val (find-bound nums target #t)])
        (if (= fst-val -1)
            '(-1 -1)
            (list fst-val
                  (find-bound nums target #f))))))

(search-range '(0 1 2 3 3 3 4 5 6 7 8) 3)
