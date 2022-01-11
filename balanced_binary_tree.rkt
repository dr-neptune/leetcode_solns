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

;; idea
;; recurse until we hit a leaf node
;; then append the depth
;; check if all the depths are within +-1

(define (leaf-depths tree [count 0])
  (if (not tree)
      (list count)
      (append (leaf-depths (tree-node-left tree) (add1 count))
              (leaf-depths (tree-node-right tree) (add1 count)))))

(define (in-range? nums)
  (cond [(empty? (rest nums)) #t]
        [(<= (abs (- (first nums) (second nums))) 1)
         (in-range? (rest nums))]))

(define (is-balanced root)
  (in-range? (leaf-depths root)))


(define lhs 2)
(define rhs 3)
