#lang racket
(require racket)

(define exls '(2 3 4 7 11))

;; naive implementation
;; traverse the list alongside a point digit a
;; if a != first ls, then remove a value from k
;; if k = 1 and a != first ls, return a as the kth missing positive number
;; if a == first ls, add1 a and rest ls

(define (find-kth-positive arr k)
  (let rc ([a 1] [arr arr] [k k])
    (cond [(empty? arr) (+ (sub1 a) k)]
          [(= a (first arr)) (rc (add1 a) (rest arr) k)]
          [(= k 1) a]
          [else (rc (add1 a) arr (sub1 k))])))

(find-kth-positive exls 5)

(find-kth-positive '(1 2 3 4) 2)


;; with binary search
;; choose the pivot index in the middle
;; if the number of positive integers which are missing is less than k (p-val - p - 1 < k)
;; then continue to search on the right side of the array
;; otherwise search on the left side of the array
;; at the end of the loop, left = right + 1 and the kth missing number is between
;; arr[right] and arr[left].
;; the number of integers missing before arr[right] = arr[right] - right - 1
;; then the number to return is arr[right] + k - (arr[right] - right - 1) = k + left

(define (find-kth-positive arr k)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [p-val (list-ref arr pivot)])
      (cond [(> left right) (+ k left)]
            [(< (sub1 (- p-val pivot)) k) (ptr-narrow (add1 pivot) right)]
            [else (ptr-narrow left (sub1 pivot))])))
  (ptr-narrow 0 (sub1 (length arr))))

(find-kth-positive exls 5)
(find-kth-positive '(1 2 3 4) 2)
