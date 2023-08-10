#lang racket
(require racket)

;; idea
;; string->list
;; take a set and convert it to a str
;; see if the entire list

;; maybe something fancy with the comparator in sort?
;; string to list to set
;; for len set, partition
;; see if every element of the resulting partitioned set is equal to set
(define (partition-n ls n)
  (if (empty? ls)
      '()
      (cons (take ls n)
            (partition-n (drop ls n) n))))

(set=? (list->set (string->list "hello"))
       (set #\e #\l #\o #\h))

;; use set=?


(define (repeated-substring-pattern s)
  )
