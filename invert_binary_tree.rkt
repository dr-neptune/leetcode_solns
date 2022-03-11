#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define extree (tree-node 4
                          (tree-node 2
                                     (make-tree-node 1)
                                     (make-tree-node 3))
                          (tree-node 7
                                     (make-tree-node 6)
                                     (make-tree-node 9))))

;; idea
;; get inorder traversal
;; reverse it

;; turns out this is a different leetcode medium problem

;; 1 2 3 4 7

;; recurse to the bottom, and flip left and right
;;

(define (invert-tree root)
  )

(define (ls->inorder-tree ls)
  ())
