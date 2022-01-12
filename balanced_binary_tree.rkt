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
(define extree2 (tree-node 1 (tree-node 2 (tree-node 3 (make-tree-node 4) (make-tree-node 4)) (make-tree-node 3)) (make-tree-node 2)))
(define extree3 (tree-node 1 (tree-node 2 (tree-node 4 (make-tree-node 8) #f) (make-tree-node 5))
                           (tree-node 3 (make-tree-node 6) #f)))
(define extree4 (tree-node 1 #f (tree-node 2 #f (make-tree-node 3))))
(define extree5 (tree-node 1 (tree-node 2 (tree-node 3 (make-tree-node 4) #f) #f)
                           (tree-node 2 #f (tree-node 3 #f (make-tree-node 4)))))

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


;; get leaf-depths working
;; if both l and r are #f, return count
(define (leaf-depths tree [count 0])
  (cond [(and (not (tree-node-left tree))
              (not (tree-node-right tree)))
         (list count)]
        [(not (tree-node-left tree))
         (leaf-depths (tree-node-right tree) (add1 count))]
        [(not (tree-node-right tree))
         (leaf-depths (tree-node-left tree) (add1 count))]
        [else
         (append (leaf-depths (tree-node-left tree) (add1 count))
                 (leaf-depths (tree-node-right tree) (add1 count)))]))

(define (in-range? nums)
  (cond [(= 1 (length nums))
         (if (< 1 (first nums)) #f #t)]
        [else
         (<= (abs (- (apply max nums)
              (apply min nums))) 1)]))

(in-range? '(2))

(define (in-range? nums)
  (<= (abs (- (apply max nums)
              (apply min nums))) 1))

(define (is-balanced-subtree root)
  (if (not (tree-node? root))
      #t
      (in-range? (leaf-depths root))))

(tree-node? (make-tree-node))

(leaf-depths extree)
(leaf-depths extree2)
(leaf-depths extree3)
(leaf-depths extree4)
(leaf-depths extree5)

(is-balanced '())
(is-balanced extree)
(is-balanced extree2)
(is-balanced extree3)
(is-balanced extree4)


(apply max (leaf-depths extree2))


;; a binary tree in which the left and right subtrees of every node differ in height by no more than 1

;; idea
;; check if every subtree is balanced
(define (is-balanced root)
  (cond [(not (tree-node? root)) #t]
        [(is-balanced-subtree root)
         (is-balanced (tree-node-left root))
         (is-balanced (tree-node-right root))]))

(leaf-depths extree2)

(define (is-balanced root)
  (cond [(not (tree-node? root)) #t]
        [else
         (append (leaf-depths root)
                 (leaf-depths (tree-node-left root))
                 (leaf-depths (tree-node-right root)))]))

(is-balanced extree)
(is-balanced extree2)
