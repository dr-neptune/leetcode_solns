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

(define extree-small (tree-node 3
                                (make-tree-node 2)
                                (make-tree-node 4)))

(define extree
  (tree-node 5
             (tree-node 3 (make-tree-node 2) (make-tree-node 4))
             (tree-node 6 #f (make-tree-node 7))))

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
    [(tree-node a #f #f) #f]
    [(or (tree-node a b #f)
         (tree-node a #f b))
     b]
    [(tree-node a b c)
     (tree-node (tree-node-val c)
                (tree-node-left root)
                (tree-node-right c))]))


(delete-node (make-tree-node 3))
(delete-node (tree-node 5 (make-tree-node 3) #f))
(delete-node (tree-node 5 #f (make-tree-node 3)))

(delete-node extree)

;; find then delete and append
(define (delete-node-reorder root)
  (match root
    [#f '()]
    [(tree-node a #f #f) #f]
    [(or (tree-node a b #f)
         (tree-node a #f b))
     b]
    [(tree-node a b c)
     (tree-node (tree-node-val c)
                (tree-node-left root)
                (tree-node-right c))]))

(define (delete-node root key)
  (if (not (tree-node? root))
      #f
      (let ([node-key (tree-node-val root)])
        (match node-key
          [(? (curry eq? key)) (delete-node-reorder root)]
          [(? (curry < key)) (tree-node node-key
                                        (delete-node (tree-node-left root) key)
                                        (tree-node-right root))]
          [_ (tree-node node-key
                        (tree-node-left root)
                        (delete-node (tree-node-right root) key))]))))


;; alternatively
;; we need to find a way to bubble up the next highest


;; we need to build up the tree as we recurse down

(delete-node extree 3)

;; fix error
(define extree2
  (tree-node 50
             (tree-node 30 #f (make-tree-node 40))
             (tree-node 70 (make-tree-node 60) (make-tree-node 80))))

;; expects 60, 30, 70, null, 40, null, 80
(delete-node extree2 50)


;; we need to do an in-order traversal until we hit the leaf
;; so that way we get the actual next highest value
(define (inorder-traversal tree)
  (if (not tree)
      '()
      (append (inorder-traversal (tree-node-left tree))
              (list (tree-node-val tree))
              (inorder-traversal (tree-node-right tree)))))

(define (min->front ds proc)
  (let* ([pre (proc ds)]
         [min-val (apply min pre)])
    (cons min-val (remove min-val pre))))


(define (delete-node-reorder root)
  (match root
    [#f '()]
    [(tree-node a #f #f) #f]
    [(or (tree-node a b #f)
         (tree-node a #f b))
     b]
    [(tree-node a b c)
     (let loop ([in-order (min->front (preorder-traversal (tree-node-left root)))])
       ;; now take our traversal and reconstruct the binary tree!
       (match in-order
         [(list a b ..1)
          (tree-node a ())])
       (tree-node (tree-node-val c)
                  (tree-node-left root)
                  (tree-node-right c)))]))

(inorder-traversal (tree-node-right extree2))

(define (build-bst-from-preorder ls)
  (match ls
    ['() #f]
    [_ (tree-node (first ls)
                  (second ls)
                  (build-bst-from-preorder (drop ls 2)))]))

(build-bst-from-preorder '(60 70 80))


(define (sorted-list->bst lst)
  (if (null? lst)
      #f
      (let* ([middle (quotient (length lst) 2)]
             [left (take lst middle)]
             [right (drop lst (+ middle 1))])
        (tree-node (list-ref lst middle)
                   (sorted-list->bst left)
                   (sorted-list->bst right)))))

(sorted-list->bst (min->front (tree-node-right extree2) preorder-traversal))

;; get min of this list and bring to front
(min->front extree2 preorder-traversal)

;; idea
;; get a traversal with min value first
;; use that min value as the root
;; build a bst off of that list with the min as the root node

;; I think we would need to build a binary tree off of the min
;; without moving the min to the front of the list in order to make this work

(define (ls->binary-tree ls)
  ;; idea
  ;; take 3 values
  ;;
  ())

(define (sorted-list->bst lst)
  (if (null? lst)
      #f
      (let* ([middle (quotient (length lst) 2)]
             [left (take lst middle)]
             [right (drop lst (+ middle 1))])
        (tree-node (list-ref lst middle)
                   (sorted-list->bst left)
                   (sorted-list->bst right)))))

(sorted-list->bst (min->front (tree-node-right extree2) preorder-traversal))

(let ([new-ls (min->front (tree-node-right extree2) preorder-traversal)])
  )


(define (list->btree lst)
  (if (null? lst)
      #f
      (tree-node (car lst)
                 (list->btree (take (cdr lst) (quotient (length (cdr lst)) 2)))
                 (list->btree (drop (cdr lst) (quotient (length (cdr lst)) 2))))))

(list->btree (min->front (tree-node-right extree2) preorder-traversal))

(define (list->btree ls)
  (if (null? ls)
      #f
      (tree-node (car ls)
                 (list->btree (cdr ls))
                 (list->btree (cddr ls)))))

(list->btree (min->front (tree-node-right extree2) preorder-traversal))


;;

;; find then delete and append
(define (delete-node-reorder root)
  (match root
    [#f '()]
    [(tree-node a #f #f) #f]
    [(or (tree-node a b #f)
         (tree-node a #f b))
     b]
    [(tree-node a b c)
     (displayln (format "~a ~a ~a" a b c))
     (match b
       [(tree-node d e f)
        (displayln (format "\t\t~a ~a ~a" d e f))
        ;; we need to loop through lefts, but let's start with just 1
        (tree-node (tree-node-val e)
                   (tree-node-left root)
                   (tree-node d #f (tree-node-val f)))]
       [(or (tree-node a b #f)
            (tree-node a #f b)) b]
       [(tree-node a #f #f) (tree-node a #f #f)]
       [_ (tree-node (tree-node-val c)
                     (tree-node-left root)
                     (tree-node-right c))])]))

(define (delete-node root key)
  (if (not (tree-node? root))
      #f
      (let ([node-key (tree-node-val root)])
        (match node-key
          [(? (curry eq? key)) (delete-node-reorder root)]
          [(? (curry < key)) (tree-node node-key
                                        (delete-node (tree-node-left root) key)
                                        (tree-node-right root))]
          [_ (tree-node node-key
                        (tree-node-left root)
                        (delete-node (tree-node-right root) key))]))))


;; alternatively
;; we need to find a way to bubble up the next highest

(delete-node extree2 50)

(delete-node extree 3)

(define (delete-node root key)
  (if (not (tree-node? root))
      #f
      (match (tree-node-val root)
        [(? (curry eq? key))
         (match root
           [(tree-node a #f #f) #f]
           [(or (tree-node a #f b)
                (tree-node a b #f))
            b]
           [_ (let loop ([rhs (tree-node-right root)])
                (let ([rlhs (tree-node-left rhs)])
                  (if (tree-node? rlhs)
                      (loop rlhs)
                      (tree-node (tree-node-val rhs)
                                 (tree-node-left root)
                                 (delete-node (tree-node-right root) (tree-node-val rhs))
                                 ))))])]
        [(? (curry < key)) (tree-node (tree-node-val root)
                                      (delete-node (tree-node-left root) key)
                                      (tree-node-right root))]
        [_ (tree-node (tree-node-val root)
                      (tree-node-left root)
                      (delete-node (tree-node-right root) key))])))

(delete-node extree 3)
(delete-node extree2 50)


(define (delete-node root key)
  (if (not (tree-node? root))
      #f
      (match (tree-node-val root)
        [(? (curry eq? key))
         (match root
           [(tree-node a #f #f) #f]
           [(or (tree-node a #f b)
                (tree-node a b #f))
            b]
           [_ (let loop ([rhs (tree-node-right root)])
                (let ([rlhs (tree-node-left rhs)])
                  (if (tree-node? rlhs)
                      (loop rlhs)
                      (tree-node (tree-node-val rhs)
                                 (tree-node-left root)
                                 (delete-node (tree-node-right root) (tree-node-val rhs))))))])]
        [(? (curry < key)) (tree-node (tree-node-val root)
                                      (delete-node (tree-node-left root) key)
                                      (tree-node-right root))]
        [_ (tree-node (tree-node-val root)
                      (tree-node-left root)
                      (delete-node (tree-node-right root) key))])))
