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

(define extree (tree-node 3
                          (tree-node 5
                                     (make-tree-node 6)
                                     (tree-node 2
                                                (make-tree-node 7)
                                                (make-tree-node 4)))
                          (tree-node 1
                                     (make-tree-node 9)
                                     (make-tree-node 8))))

(define extree2 (tree-node 3
                           (tree-node 5
                                      (make-tree-node 6)
                                      (make-tree-node 7))
                           (tree-node 1
                                      (make-tree-node 4)
                                      (tree-node 2
                                                 (make-tree-node 9)
                                                 (make-tree-node 8)))))

(define (leaf-node? tree)
  (and (false? (tree-node-left tree))
       (false? (tree-node-right tree))))

(define (leaf-nodes tree)
  (filter number?
          (if (tree-node? tree)
              (cond [(leaf-node? tree) (list (tree-node-val tree))]
                    [else (append (leaf-nodes (tree-node-left tree))
                                  (leaf-nodes (tree-node-right tree)))])
              '())))

(define (leaf-similar root1 root2)
  (equal? (leaf-nodes root1)
          (leaf-nodes root2)))

(leaf-similar (tree-node 1 (make-tree-node 2) #f)
              (tree-node 2 (make-tree-node 2) #f))
