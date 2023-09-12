#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

;; idea
;; use bsearch from previous question
;; if found, use `delete-node`

(define extree (tree-node 3
                          (make-tree-node 2)
                          (make-tree-node 4)))


;; case
;; if node has no children then do nothing
;; if node has 1 child, replace it with the child
;; if node has 2 children,
;;   find the next larger or smaller value,
;;   replace the node with that value, and delete the
;;   next larger or smaller, etc

(define (delete-node root)
  (match root
    [#f '()]
    [(tree-node a #f #f)]
    [(or (tree-node a b #f)
         (tree-node a #f b))
     b]
    [else ;; (tree a b c)
     ]))
