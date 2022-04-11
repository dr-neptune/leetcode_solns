#lang racket
(require racket)

;; naive
;; grab the length of the list
;; and then iterate from 0 -> (length ls)
;; for each value v, check if there are exactly v elements in the list where (<= e v)
;; if we run out of numbers, return -1

(define (correct-count ls val)
  (foldl + 0
         (map (λ (x) (or (and x 1) 0))
              (map (λ (x) (>= x val)) ls))))


(define (special-array nums)
  (let rc ([max-len (length nums)]
           [k 0])
    (cond [(> k max-len) -1]
          [(= k (correct-count nums k)) k]
          [else (rc max-len (add1 k))])))

(special-array exls)
(special-array '(0 0))
(special-array '(0 4 3 0 4))

;; with binary search
;; first we get a boundary [0, length ls]
;; then we take the mid point
;; if (> (correct-count ls mid-point) mid-point) then try on the RHS
;; else try on the lhs
;; if (>= left right) return -1
;; if (= (correct-count ls mid-point) mid-point), return mid-point

(define (correct-count ls val)
  (foldl + 0
         (map (λ (x) (or (and x 1) 0))
              (map (λ (x) (>= x val)) ls))))

(define (special-array nums)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [check (correct-count nums mid)])
      (cond [(= check mid) mid]
            [(>= left right) -1]
            [(> check mid) (ptr-narrow (add1 mid) right)]
            [else (ptr-narrow left (sub1 mid))])))
  (ptr-narrow 0 (length nums)))

(special-array exls)

(special-array '(0 0))
(special-array '(0 4 3 0 4))
