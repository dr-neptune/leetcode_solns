#lang racket
(require racket)


#|

idea

inorder traversal

monotonically increasing

|#

(require (only-in srfi/1 map))

(define (inorder-traversal tree)
  (if (not tree)
      '()
      (append (inorder-traversal (tree-node-left tree))
              (list (tree-node-val tree))
              (inorder-traversal (tree-node-right tree)))))

(define (is-valid-bst root)
  (let ([inorder (inorder-traversal extree)])
    (andmap identity (map (λ (a b) (<= a b)) inorder (rest inorder)))))


(let ([inorder '(2 2 2)])
  (andmap identity (map (λ (a b) (<= a b)) inorder (rest inorder))))
