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


;; top down approach
(define (height root)
  (if (not (tree-node? root))
      0
      (add1 (max (height (tree-node-left root))
                 (height (tree-node-right root))))))


(define (is-balanced root)
  (if (not (tree-node? root))
      #t
      (let ([left (height (tree-node-left root))]
            [right (height (tree-node-right root))])
        (and (<= (abs (- left right)) 1)
             (is-balanced (tree-node-left root))
             (is-balanced (tree-node-right root))))))


;; another solution
(define (is-balanced root)
 (define (balanced? root [height 0])
  (if (not (tree-node? root))
      height
      (let ([left (balanced? (tree-node-left root) (add1 height))]
            [right (balanced? (tree-node-right root) (add1 height))])
        (if (or (< left 0) (< right 0) (> (abs (- left right)) 1))
            -1
            (max left right)))))
  (>= (balanced? root) 0))


(module+ test
  (require rackunit)
  (check-true (is-balanced extree))
  (check-false (is-balanced extree2))
  (check-false (is-balanced extree3))
  (check-true (is-balanced extree4)))
