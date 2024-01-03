#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))


(define (sorted-list->bst lst)
  (let ([len (length lst)])
    (if (not (zero? len))
        (let* ([mid (quotient len 2)]
               [left-list (take lst mid)]
               [right-list (drop lst (add1 mid))]
               [root (make-tree-node (list-ref lst mid))])
            (set-tree-node-left! root (sorted-list->bst left-list))
            (set-tree-node-right! root (sorted-list->bst right-list))
            root)
        #f)))



(define preorder '(3 9 20 15 7))
(define inorder '(9 3 15 20 7))

(sorted-list->bst inorder)

#|

idea



|#
