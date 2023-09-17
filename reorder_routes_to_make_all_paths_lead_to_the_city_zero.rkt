#lang racket
(require racket)

;; idea
;; for each node, see where it can reach
;; if it can't reach node 0
;; then add it to a list

(define exgraph '((0 1) (1 3) (2 3) (4 0) (4 5)))

(sort exgraph #:key car <)

;; idea
;; if a number can't get there
;; flip the order of the pair
;; then check if it can reach there
;; if not, go to a different node
(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

;; (define states exgraph)

;; (car-find 0 exgraph)

(define (can-reach-0 node)
  (match node
    [#f #f]
    [(list 0 b) #t]
    [(list a b) (can-reach-0 (car-find b states))]))

(car-find 0 exgraph)

(can-reach-0 '(4 0))
(can-reach-0 '(2 3))
()

(count (compose not identity) (map can-reach-0 exgraph))


(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

(define (min-reorder n connections)
  (define (can-reach-0 node)
    (match node
      [#f #f]
      [(list 0 b) #t]
      [(list a b) (can-reach-0 (car-find b connections))]))
  (map can-reach-0 connections)
  ;(count (compose not identity) (map can-reach-0 connections))
  )

(define exgraph2 '((1 0) (1 2) (3 2) (3 4)))

(min-reorder 5 exgraph2)



;; (let ([connections exgraph]
;;       [n (length exgraph)])
;;   (define (dfs node)
;;     (let ([seen (make-vector n 0)])
;;       ())))

(can-reach-0 '(1 0))

(min-reorder 5 exgraph2)



(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

(define (can-reach-0 node states)
  (match node
    [#f #f]
    [(list b 0) #t]
    [(list a b) (can-reach-0 (car-find b states) states)]))



(define (min-reorder n connections)
  (define (can-reach-0 node)
    (match node
      [#f #f]
      [(list 0 b) #t]
      [(list a b) (can-reach-0 (car-find b connections))]))
  ;; idea
  ;; map over nodes
  ;; if we get a #f, try swapping the values and check again
  (let loop ([paths (map can-reach-0 connections)]
             [num-changes 0])
    ;; get indexes
    (let ([f-idx (index-of paths #f)])
      (displayln (format "~a ~a" paths f-idx))
      (match f-idx
        [#f num-changes]
        [_ (loop (map can-reach-0 (list-update connections f-idx reverse))
                 (add1 num-changes))]))))


(can-reach-0 '(1 3) (list-update exgraph 1 reverse))

(min-reorder 6 exgraph)

(list-update '((1 2)(3 4)(5 6)) 1 reverse)

(index-of (map (curryr can-reach-0 exgraph2) exgraph2) #f)



;; retry

;; idea
;; start from 0
;; if 0 is not second, transpose the arrow
;; (0, 1) -> (1, 0)
;; then store the first value and go to whatever nodes connect to it
;; (1, 3)
;; if the first value from the previous node isn't second, transpose
;; (3, 1)
;; and then store the first value and go to whatever nodes connect to it
;; (2, 3)
;; if the first value from the previous node is second then we are good
;; store the first value, and find nodes that contain it
;; if nothing contains it, return #f


(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num)
    (indexes-where connections (λ (pair) (member node-num pair))))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)]
               [pivot-node 0])
      (if (> pivot-node n)
          num-swaps
          (begin
            (for ([i relevant-nodes])
              (let ([vec-elements (vector-ref graph i)])
                (when (eq? pivot-node (second vec-elements))
                  (begin
                    (vector-set! graph i (reverse vec-elements))
                    (set! num-swaps (add1 num-swaps))
                    (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))))))
            (loop (get-indices (add1 pivot-node)) (add1 pivot-node)))))))


(indexes-where exgraph (λ (pair) (member 10 pair)))


;; still start with 0
;; but once we do that, if we swap, loop on the new first value

(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num [ignore #f])
    (let ([idxs (indexes-where connections (λ (pair) (member node-num pair)))])
      (if ignore
          (remove ignore idxs)
          idxs)))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)]
               [pivot-node 0])
      (if (> pivot-node n)
          num-swaps
          (begin
            (for ([i relevant-nodes])
              (let ([vec-elements (vector-ref graph i)])
                (when (eq? pivot-node (first vec-elements))
                  (begin
                    (vector-set! graph i (reverse vec-elements))
                    (set! num-swaps (add1 num-swaps))
                    (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))
                    (loop (get-indices (second vec-elements) i) (second vec-elements)))))))))))

;; now we need to account for the 0 <- 4 -> 5 branch
(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num [ignore #f])
    (let ([idxs (indexes-where connections (λ (pair) (member node-num pair)))])
      (if ignore
          (remove ignore idxs)
          idxs)))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)]
               [pivot-node 0])
      (if (> pivot-node n)
          num-swaps
          (begin
            (for ([i relevant-nodes])
              (let ([vec-elements (vector-ref graph i)])
                (when (eq? pivot-node (first vec-elements))
                  (begin
                    (vector-set! graph i (reverse vec-elements))
                    (set! num-swaps (add1 num-swaps))
                    (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))
                    (loop (get-indices (second vec-elements) i) (second vec-elements)))))))))))

;; we also need to remove the machinery that counts upwards
(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num [ignore #f])
    (let ([idxs (indexes-where connections (λ (pair) (member node-num pair)))])
      (if ignore
          (remove ignore idxs)
          idxs)))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)])
      (for ([i relevant-nodes])
          (let ([vec-elements (vector-ref graph i)])
            (when (eq? pivot-node (first vec-elements))
              (begin
                (vector-set! graph i (reverse vec-elements))
                (set! num-swaps (add1 num-swaps))
                (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))
                (loop (get-indices (second vec-elements) i)))))))))


;; restart
;; try traversing the graph
(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

(let ([connections exgraph])
  (let loop ([curr-node (first connections)])
    (if curr-node
        (let ([fn (first curr-node)]
              [sn (second curr-node)])
          (displayln (format "visiting: ~a" fn))
          (let ([nn (car-find sn connections)])
            (loop nn)
            ;; (if (not nn)
            ;;     (loop (car-find fn connections))
            ;;     (loop nn))
            ))
        '()
        )))

;; ah wah ah ah ah
