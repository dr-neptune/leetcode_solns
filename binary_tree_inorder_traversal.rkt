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


(define (inorder-traversal tree)
  (if (not tree)
      '()
      (append (inorder-traversal (tree-node-left tree))
              (list (tree-node-val tree))
              (inorder-traversal (tree-node-right tree)))))

(define (sorted-list->inverted-bst lst)
  (let ([len (length lst)])
    (if (not (zero? len))
        (let* ([mid (quotient len 2)]
               [left-list (take lst mid)]
               [right-list (drop lst (add1 mid))]
               [root (make-tree-node (list-ref lst mid))])
            (set-tree-node-right! root (sorted-list->inverted-bst left-list))
            (set-tree-node-left! root (sorted-list->inverted-bst right-list))
            root)
        #f)))

(define/contract (invert-tree root)
  (-> (or/c tree-node? #f) (or/c tree-node? #f))
  (if (empty? root)
      root
      ((compose sorted-list->inverted-bst inorder-traversal) root)))


(invert-tree extree)
