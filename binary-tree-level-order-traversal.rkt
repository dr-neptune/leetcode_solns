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


(define extree (tree-node 3 (make-tree-node 9) (tree-node 20 (make-tree-node 15) (make-tree-node 7))))

;; new idea
;; recurse, keep a level counter
;; append val + level
;; group by level
;; do a pre-order traversal
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

(level-order extree)
