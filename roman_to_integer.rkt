#lang racket
(require racket)

;; idea
;; make a hash map with the basics and the subtraction rules
;; parse the string, checking the value and the value ahead
;; if none of the subtraction rules hold for the next 2 digits,
;; take the current value and add it to the total. Otherwise
;; take the subtraction value and add it to the total and move
;; ahead 2 values
