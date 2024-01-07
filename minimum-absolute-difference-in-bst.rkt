#lang racket
(require racket)

#|

idea

bst -> preorder -> min diff

|#


; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define extree (tree-node 4 (tree-node 2 (make-tree-node 1) (make-tree-node 3))
                          (make-tree-node 6)))

(define extree (tree-node 543 (tree-node 384 #f (make-tree-node 445))
                          (tree-node 652 #f (make-tree-node 699))))

(define (inorder-traversal tree)
  (if (not tree)
      '()
      (append (inorder-traversal (tree-node-left tree))
              (list (tree-node-val tree))
              (inorder-traversal (tree-node-right tree)))))

(define (get-minimum-difference root)
  (abs (apply - (take (inorder-traversal root) 2))))

(require (only-in srfi/1 map))

(define (get-minimum-difference root)
  (let ([inorder (inorder-traversal root)])
    (apply min
           (for/list ([combo (in-combinations inorder 2)])
             (abs (apply - combo))))))



(let ([inorder (inorder-traversal extree)])
  (apply min
         (for/list ([combo (in-combinations inorder 2)])
           (abs (apply - combo)))))


(require (only-in srfi/1 map))

(inorder-traversal extree)

(let ([inorder (inorder-traversal extree)])
  (apply min (map (compose abs (curry apply -) list) inorder (rest inorder))))

(define (get-minimum-difference root)
  (let ([inorder (inorder-traversal root)])
    (apply min (map (compose abs (curry apply -) list) inorder (rest inorder)))))
