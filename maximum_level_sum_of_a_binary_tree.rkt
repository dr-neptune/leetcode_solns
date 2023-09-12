#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))


;; idea
;; go layer by layer, adding up values
;; get the layer with the max val
(define extree (tree-node 1
                          (tree-node 7
                                     (make-tree-node 7)
                                     (make-tree-node -8))
                          (make-tree-node 0)))


;; (define (layer-vals-sum layer)
;;   (let iter ([nodes layer]
;;              [val-sum 0])
;;     (displayln (format "~a ~a" nodes val-sum))
;;     (match nodes
;;       [(tree-node a b c) a]
;;       [(list (tree-node a b c) ...)
;;        (iter (rest nodes) (+ a val-sum))]
;;       [_ val-sum])))


(define (layer-vals-sum layer-ls)
  (foldl (λ (a b) (+ (tree-node-val a) b)) 0 layer-ls))


;; (layer-vals-sum (make-layer-node-list (list extree)))

;; make list of nodes at level
(define (make-next-layer-node-list root)
  (match root
    [(tree-node a #f #f) '()]
    ;; [(tree-node a #f #f) '()]
    [(or (tree-node a b #f)
         (tree-node a #f b)) (list b)]
    [(tree-node a b c) (list b c)]))

(map tree-node-val (make-next-layer-node-list (tree-node-left extree)))


;; now iterate over layers
(sort (let loop ([root extree]
                 [depth-maxes '()]
                 [depth 0])
        (if (not (tree-node? root))
            depth-maxes
            (let ([max-info (list (apply + (map tree-node-val (make-next-layer-node-list root))) depth)])
              (append (loop (tree-node-left root) (cons max-info depth-maxes) (add1 depth))
                      (loop (tree-node-right root) (cons max-info depth-maxes) (add1 depth))))))
      (λ (a b) (< (cadr a) (cadr b))))

;; account for whether there is another level or not


(index-where (sort (let loop ([root extree]
                 [depth-maxes `((,(tree-node-val extree) 0))]
                 [depth 1])
        (match root
          [(tree-node a #f #f) depth-maxes]
          [_ (let ([max-info (list (apply + (map tree-node-val (make-next-layer-node-list root))) depth)])
               (set-union (loop (tree-node-left root) (cons max-info depth-maxes) (add1 depth))
                          (loop (tree-node-right root) (cons max-info depth-maxes) (add1 depth))))]))
                #:key cadr <)
          (λ (ls) (eq? (car ls) (apply max (map first '((1 0) (7 1) (-1 2)))))))



(define (make-next-layer-node-list root)
  (match root
    [(tree-node a #f #f) '()]
    [#f '()]
    [(or (tree-node a b #f)
         (tree-node a #f b)) (list b)]
    [(tree-node a b c) (list b c)]))

(define (layer-sum layer-ls) (apply + (map tree-node-val layer-ls)))

(define (get-depth-sums root)
  (sort (let loop ([tree root]
                   [depth-maxes `((,(tree-node-val root) 1))]
                   [depth 2])
          (match tree
            [#f depth-maxes]
            [(tree-node a #f #f) depth-maxes]
            [_ (let ([max-info (list (layer-sum (make-next-layer-node-list tree)) depth)])
                 (append (loop (tree-node-left tree) (cons max-info depth-maxes) (add1 depth))
                         (loop (tree-node-right tree) (cons max-info depth-maxes) (add1 depth))))]))
        #:key cadr <))


(define (max-level-sum root)
  (let* ([res (get-depth-sums root)]
         [max-val (apply max (map first res))])
    (cadr (list-ref res (index-of (map car res) max-val)))))


(max-level-sum extree3)

;; this is broken
;; with ; match: no matching clause for #f
(define extree2 (tree-node 989
                           #f
                           (tree-node 10250
                                      (make-tree-node 98693)
                                      (tree-node -89388
                                                 #f
                                                 (make-tree-node -32127)))))

;; not done!


(define extree3
  (tree-node 1
             (tree-node 1 (make-tree-node 7) (make-tree-node -8))
             (tree-node 0 (make-tree-node -7) (make-tree-node 9))))

;; again

;; maybe start with just creating a list of values at each level
(define (make-next-layer-node-list root)
  (match root
    [(tree-node a #f #f) '()]
    [#f '()]
    [(or (tree-node a b #f)
         (tree-node a #f b)) (list b)]
    [(tree-node a b c) (list b c)]))

(map tree-node-val (make-next-layer-node-list extree3))

(apply + (append (map tree-node-val (make-next-layer-node-list (tree-node-left extree3)))
                 (map tree-node-val (make-next-layer-node-list (tree-node-right extree3)))))

(make-next-layer-node-list (make-next-layer-node-list extree3))


;; so for a single node, we get 2 children
;; then we need to iterate
(flatten (map make-next-layer-node-list (make-next-layer-node-list extree3)))

(empty? (flatten (map make-next-layer-node-list
 (flatten (map make-next-layer-node-list (make-next-layer-node-list extree3))))))


(define inter
  (let loop ([depth 1]
           [levels '()]
           [next-level (list extree3)])
  (if (empty? next-level)
      (reverse levels)
      (loop (add1 depth)
            (cons (list (apply + (map tree-node-val next-level)) depth) levels)
            (flatten (map make-next-layer-node-list next-level))))))

(define (get-layer-sums tree)
  (let loop ([depth 1]
             [levels '()]
             [next-level (list tree)])
    (if (empty? next-level)
        (reverse levels)
        (loop (add1 depth)
              (cons (list (apply + (map tree-node-val next-level)) depth) levels)
              (flatten (map make-next-layer-node-list next-level))))))

;; now we want to get the max that mins the depth

(cadr (list-ref inter (apply min(indexes-of (map car inter) (apply max (map car inter))))))


;; for a tree, return a list of all the children nodes
(define (make-next-layer-node-list root)
  (match root
    [(tree-node a #f #f) '()]
    [#f '()]
    [(or (tree-node a b #f)
         (tree-node a #f b)) (list b)]
    [(tree-node a b c) (list b c)]))


;; returns a list of values at each level and depths
;; i.e. (((1 2 3 4 5) 0) ((6 7 8) 1) ((9 10 11) 2))
(define (get-layer-values tree)
  (let loop ([depth 1]
             [levels '()]
             [next-level (list tree)])
    (if (empty? next-level)
        (reverse levels)
        (loop (add1 depth)
              (cons (list (map tree-node-val next-level) depth) levels)
              (flatten (map make-next-layer-node-list next-level))))))

;; for a layer values list, apply proc to the values
(define (fold-tree-val-layer tree [proc +])
  (map (λ (ls) (list (apply proc (car ls)) (cdr ls)))
       (get-layer-values tree)))

(define (max-level-sum root)
  (let* ([layer-sums-and-depths (fold-tree-val-layer root)]
         [layer-sums (map car layer-sums-and-depths)])
    (caadr
     (list-ref layer-sums-and-depths
               (apply min (indexes-of layer-sums
                                      (apply max layer-sums)))))))

(max-level-sum extree3)
(max-level-sum extree)
(max-level-sum extree2)
