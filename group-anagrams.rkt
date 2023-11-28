#lang racket
(require racket)

(define exls '("eat" "tea" "tan" "ate" "nat" "bat"))

(define (group-anagrams strs)
  (group-by (λ (s) (list->string (sort (string->list s) char<?))) strs))

(group-anagrams exls)
