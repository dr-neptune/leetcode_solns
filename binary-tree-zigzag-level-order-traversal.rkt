#lang racket
(require racket)

#|

idea

level order, then for odd indices reverse

|#



(define (preorder-traversal root [level 0])
  (if (not (tree-node? root))
      '()
      (cons
       (cons (list (tree-node-val root) level)
             (preorder-traversal (tree-node-left root) (add1 level)))
       (preorder-traversal (tree-node-right root) (add1 level)))))

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (level-order root)
  (if (not (tree-node? root))
      '()
      (let ([pre (preorder-traversal root)])
        (map (λ (ls) (map first ls))
             (group-by (λ (x) (second x)) (split-into (flatten pre) 2))))))


(define extree (tree-node 3 (make-tree-node 9) (tree-node 20 (make-tree-node 15) (make-tree-node 7))))


(let ([l-order (level-order extree)])
  (for/list ([idx (in-range (length l-order))]
             [level l-order])
    (if (odd? idx)
        (reverse level)
        level)))



(define (preorder-traversal root [level 0])
  (if (not (tree-node? root))
      '()
      (cons
       (cons (list (tree-node-val root) level)
             (preorder-traversal (tree-node-left root) (add1 level)))
       (preorder-traversal (tree-node-right root) (add1 level)))))

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (level-order root)
  (if (not (tree-node? root))
      '()
      (let ([pre (preorder-traversal root)])
        (map (λ (ls) (map first ls))
             (group-by (λ (x) (second x)) (split-into (flatten pre) 2))))))

(define (zigzag-level-order root)
  (let ([l-order (level-order root)])
    (for/list ([idx (in-range (length l-order))]
               [level l-order])
      (if (odd? idx)
          (reverse level)
          level))))

(level-order extree)
