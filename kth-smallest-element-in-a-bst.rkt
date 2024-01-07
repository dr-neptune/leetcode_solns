#lang racket
(require racket)

#|

idea

inorder -> take kth item

|#

(define (inorder-traversal tree)
  (if (not tree)
      '()
      (append (inorder-traversal (tree-node-left tree))
              (list (tree-node-val tree))
              (inorder-traversal (tree-node-right tree)))))

(define (kth-smallest root k)
  (let ([inorder (inorder-traversal root)])
    (list-ref inorder (sub1 k))))
