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

(define extree (tree-node 1 #f (tree-node 2 (make-tree-node 3) #f)))

;; preorder is root left right
(define (preorder-traversal root)
  (if (not (tree-node? root))
      '()
      (append (list (tree-node-val root))
              (preorder-traversal (tree-node-left root))
              (preorder-traversal (tree-node-right root)))))

(preorder-traversal extree)
