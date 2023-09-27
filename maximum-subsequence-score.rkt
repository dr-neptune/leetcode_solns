#lang racket
(require racket)

;; idea
;; try to get an amortized linear top k match for indices in nums2
;; then match to nums1

;; start off by getting top k in nums2 and recording which index they are in
;; maybe a heap of associative lists?
