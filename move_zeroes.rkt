#lang racket
(require racket)

;; idea
;; make a special sorter that treats 0 as a huge number
;; then sort the list
(define (~< a b)
  (cond [(and (zero? a) (zero? b)) #t]
        [(zero? a) #f]
        [(zero? b) #t]
        [else (< a b)]))

(define (move-zeroes nums)
  (sort nums ~<))

(define exls '(0 1 0 3 12))

(sort exls ~<)

;; the whole in-place shpiel is pretty lame
