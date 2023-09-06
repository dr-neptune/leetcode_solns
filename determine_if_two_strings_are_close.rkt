#lang racket
(require racket)

;; op1 allows you to freely reorder the string
;; op2 allows you to freely change the counts of the letters

;; idea
;; check if same length and consisting of the same letters
;; then
;; do op2 first until the frequencies match
;; then do op1 to make the values match
