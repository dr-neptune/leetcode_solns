#lang racket
(require racket)

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))


(define extree (tree-node 5 (tree-node 4 (tree-node 11 (make-tree-node 7) (make-tree-node 2)) #f)
                          (tree-node 8 (make-tree-node 13) (tree-node 4 #f (make-tree-node 1)))))

(define extree2 (tree-node 1 (make-tree-node 2)
                           (make-tree-node 3)))

;; idea
;; depth first search
;; for each jump, subtract the node amounts from the desired sum
;; if sum == 0, return #t
