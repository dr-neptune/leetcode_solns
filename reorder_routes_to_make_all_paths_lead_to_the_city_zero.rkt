#lang racket
(require racket)

;; idea
;; for each node, see where it can reach
;; if it can't reach node 0
;; then add it to a list

(define exgraph '((0 1) (1 3) (2 3) (4 0) (4 5)))

(sort exgraph #:key car <)

;; idea
;; if a number can't get there
;; flip the order of the pair
;; then check if it can reach there
;; if not, go to a different node
(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

;; (define states exgraph)

;; (car-find 0 exgraph)

(define (can-reach-0 node)
  (match node
    [#f #f]
    [(list 0 b) #t]
    [(list a b) (can-reach-0 (car-find b states))]))

(car-find 0 exgraph)

(can-reach-0 '(4 0))
(can-reach-0 '(2 3))

(count (compose not identity) (map can-reach-0 exgraph))


(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

(define (min-reorder n connections)
  (define (can-reach-0 node)
    (match node
      [#f #f]
      [(list 0 b) #t]
      [(list a b) (can-reach-0 (car-find b connections))]))
  (count (compose not identity) (map can-reach-0 connections)))

(define exgraph2 '((1 0) (1 2) (3 2)))

;; (let ([connections exgraph]
;;       [n (length exgraph)])
;;   (define (dfs node)
;;     (let ([seen (make-vector n 0)])
;;       ())))

(min-reorder 5 exgraph2)
