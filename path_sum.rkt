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


(define extree (tree-node 5 (tree-node 4 (tree-node 11 (make-tree-node 7) (make-tree-node 2)) #f)
                          (tree-node 8 (make-tree-node 13) (tree-node 4 #f (make-tree-node 1)))))

(define extree2 (tree-node 1 (make-tree-node 2)
                           (make-tree-node 3)))

(define extree3 (tree-node 1 (make-tree-node 2) #f))

;; idea
;; for each jump, subtract the node amounts from the desired sum
;; if sum == val, return #t
(define (leaf-node? tree)
  (and (false? (tree-node-left tree))
       (false? (tree-node-right tree))))


(define (has-path-sum root targetSum)
  (cond [(not (tree-node? root)) #f]
        [(and (= (tree-node-val root) targetSum)
              (leaf-node? root)) #t]
        [else (or (has-path-sum (tree-node-left root) (- targetSum (tree-node-val root)))
                  (has-path-sum (tree-node-right root) (- targetSum (tree-node-val root))))]))


(module+ test
  (require rackunit)
  (check-true (has-path-sum extree 22))
  (check-false (has-path-sum extree2 5))
  (check-false (has-path-sum '() 0))
  (check-false (has-path-sum extree3 1)))
