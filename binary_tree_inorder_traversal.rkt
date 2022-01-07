#lang racket
(require racket)

; Definition for a binary tree node.

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))


(define extree (tree-node 1 #f (tree-node 2 (tree-node 3 #f #f) #f)))

;; in-order is left root right

(define (tree-traversal tree)
  (let ([left (tree-node-left tree)]
        [root (tree-node-val tree)]
        [right (tree-node-right tree)])
    (cond [left (if (tree-node? left)
                    (tree-traversal left)
                    (tree-node-val left))]
          [root (tree-node-val root)]
          [right (if (tree-node? right)
                    (tree-traversal right)
                    (tree-node-val right))]
          [else (make-tree-node)])))


(tree-traversal extree)

;; tree traversal left root right
(tree-node-left extree)
(tree-node-val extree)
(tree-node-right extree)
