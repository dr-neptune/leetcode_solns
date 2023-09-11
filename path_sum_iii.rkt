#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))


(define extree (tree-node
                10
                (tree-node 5
                           (tree-node 3
                                      (make-tree-node 3)
                                      (make-tree-node -2))
                           (tree-node 2
                                      #f
                                      (make-tree-node 1)))
                (tree-node -3
                           #f
                           (make-tree-node 11))))


(define extree2
  (tree-node 5
             (tree-node 4
                        (tree-node 11
                                   (make-tree-node 7)
                                   (make-tree-node 2))
                        #f)
             (tree-node 8
                        (make-tree-node 13)
                        (tree-node 4
                                   (make-tree-node 5)
                                   (make-tree-node 1)))))


(let ([target 8])
  (let outer ([tree extree3])
    (let loop ([curr-tree-root tree]
               [curr-sum 0])
      (if (not (tree-node? curr-tree-root))
          (list 0)
          (let ([inter-sum (+ curr-sum (tree-node-val curr-tree-root))])
            (cond [(= target inter-sum) (begin
                                          (displayln (format "root: ~a sum: ~a" curr-tree-root curr-sum))
                                          (list 1))]
                  [(< target inter-sum)
                   (append (outer (tree-node-right tree))
                           (outer (tree-node-left tree)))]
                  [(> target inter-sum)
                   (append (loop (tree-node-left curr-tree-root) inter-sum)
                           (loop (tree-node-right curr-tree-root) inter-sum))]))))))

(define (path-sum root targetSum)
  ((compose (curry count list?) remove-duplicates)
         (let outer ([tree root])
           (let loop ([curr-tree-root tree]
                      [curr-sum 0])
             (if (not (tree-node? curr-tree-root))
                 (list 0)
                 (let ([inter-sum (+ curr-sum (tree-node-val curr-tree-root))])
                   (cond [(= targetSum inter-sum) (begin
                                                    (displayln (format "root: ~a sum: ~a" curr-tree-root curr-sum))
                                                    `((,(tree-node-val curr-tree-root) ,curr-sum)))]
                         [(< targetSum inter-sum)
                          (append (outer (tree-node-right tree))
                                  (outer (tree-node-left tree)))]
                         [(> targetSum inter-sum)
                          (append (loop (tree-node-left curr-tree-root) inter-sum)
                                  (loop (tree-node-right curr-tree-root) inter-sum))])))))))

;; it duplicates because it sees the 7 15 bit twice
;; for both root = 5 and root = 4
;; hacky tactic
;; append a list of the tree-val and sum
;; delete duplicates
;; see if it works



(path-sum extree 8)
(path-sum extree2 22)

(count list? (remove-duplicates (path-sum extree2 22)))

;; this doesn't work because our sum is always 1 node behind
(define extree3 (tree-node -2 #f (make-tree-node -3)))
(path-sum extree3 -5)

;; make it more `eager`
;; oh damn, it might be because of double negatives
;;


(define (path-sum root targetSum)
  (define (find-paths root proc1 proc2)
    (identity ;(compose (curry count list?) remove-duplicates)
     (let outer ([tree root])
       (displayln (format "new root: ~a" tree))
           (let inner ([curr-tree-root tree]
                       [curr-sum 0])
             (displayln (format "recursing. root: ~a sum: ~a" curr-tree-root curr-sum))
             (if (not (tree-node? curr-tree-root))
                 '()
                 (let ([inter-sum (+ curr-sum (tree-node-val curr-tree-root))])
                   (cond [(= targetSum inter-sum) `((,(tree-node-val curr-tree-root) ,curr-sum))]
                         [(proc1 targetSum inter-sum)
                          (append (outer (tree-node-right tree))
                                (outer (tree-node-left tree)))]
                         [(proc2 targetSum inter-sum)
                          (append (inner (tree-node-left curr-tree-root) inter-sum)
                                  (inner (tree-node-right curr-tree-root) inter-sum))])))))))
  (if (negative? targetSum)
      (find-paths root > <)
      (find-paths root < >)))


(path-sum extree3 -5)
(path-sum extree2 22)
(path-sum extree 8)


(define extree4 (tree-node 1 (make-tree-node -2) (make-tree-node -3)))

(path-sum extree4 -2)

;; try again, but this time don't worry about <, >. Just worry about testing all paths

(let ([root extree4] [targetSum -2])
  (let outer ([tree root])
    (displayln (format "new root: ~a" tree))
    (let inner ([curr-tree-root tree]
                [curr-sum 0])
      (if (not (tree-node? curr-tree-root))
          '()
          (let ([inter-sum (+ curr-sum (tree-node-val curr-tree-root))])
            (displayln (format "~a ~a" inter-sum curr-tree-root))
            (cond [(= targetSum inter-sum) (begin
                                             (displayln (format "success! ~a ~a ~a" inter-sum curr-sum (tree-node-val curr-tree-root)))
                                             (list 1))]
                  [(< targetSum curr-sum)
                   (append (outer (tree-node-left tree))
                           (outer (tree-node-right tree)))]
                  [else (append (inner (tree-node-left curr-tree-root) inter-sum)
                                (inner (tree-node-left curr-tree-root) inter-sum))]))))))




;; maybe try giving this to chatgpt?
Given the root of a binary tree and an integer targetSum, return the number of paths where the sum of the values along the path equals targetSum.

The path does not need to start or end at the root or a leaf, but it must go downwards (i.e., traveling only from parent nodes to child nodes).

Input: root = [10,5,-3,3,2,null,11,3,-2,null,1], targetSum = 8
Output: 3

Input: root = [5,4,8,11,null,13,4,7,2,null,null,5,1], targetSum = 22
Output: 3



(define (path-sum root target-sum)
  (define (dfs node curr-sum)
    (cond
      [(not (tree-node? node)) 0]
      [(= curr-sum target-sum) 1]
      [else (+ (dfs (tree-node-left node) (+ curr-sum (tree-node-val node)))
               (dfs (tree-node-right node) (+ curr-sum (tree-node-val node))))]))

  (define (outer node)
    (cond
      [(not (tree-node? node)) 0]
      [else (+ (dfs node 0)
               (outer (tree-node-left node))
               (outer (tree-node-right node)))]))

  (outer root))


(path-sum extree 8)
(path-sum extree2 22)
(path-sum extree3 -5)
(path-sum extree4 -2)



;; idea


(let ([root extree4]
      [targetSum -2])
  (define (dfs node curr-sum)
    ;(displayln (format "curr: ~a ~a" node curr-sum))
    (cond [(= curr-sum targetSum) (begin
                                    (displayln (format "success! ~a ~a" node curr-sum))
                                    (list 1))]
          [(not (tree-node? node)) '()]
          [else (append (dfs (tree-node-left node) (+ curr-sum (tree-node-val node)))
                        (dfs (tree-node-right node) (+ curr-sum (tree-node-val node))))]))
  (define (outer node)
    (displayln (format "new root: ~a" node))
    (cond [(not (tree-node? node)) '()]
          [else (append (dfs node 0)
                        (outer (tree-node-left node))
                        (outer (tree-node-right node)))]))
  (outer root))


(define (path-sum root targetSum)
  (define (dfs node curr-sum)
    (displayln (format "curr: ~a ~a" node curr-sum))
    (cond [(not (tree-node? node)) 0]

          [(= curr-sum targetSum) (begin
                                    (displayln (format "success! ~a ~a" node curr-sum))
                                    1)]

          [else (+ (dfs (tree-node-left node) (+ curr-sum (tree-node-val node)))
                   (dfs (tree-node-right node) (+ curr-sum (tree-node-val node))))]))

  (define (outer node)
    (displayln (format "new root: ~a" node))
    (cond [(not (tree-node? node)) 0]
          [else (+ (dfs node 0)
                   (outer (tree-node-left node))
                   (outer (tree-node-right node)))]))

  (outer root))


(define (path-sum root targetSum)
  (define (dfs node curr-sum)
    (if (not (tree-node? node))
        0
        (let ([inter-sum (+ curr-sum (tree-node-val node))])
          (if (= inter-sum targetSum)
              (begin
                (displayln (format "~a \t ~a \t ~a" curr-sum inter-sum node))
                1)
              (+ (dfs (tree-node-left node) (+ curr-sum (tree-node-val node)))
                 (dfs (tree-node-right node) (+ curr-sum (tree-node-val node))))))))
  (define (outer node)
    (displayln (format "new root: ~a" node))
    (cond [(not (tree-node? node)) 0]
          [else (+ (dfs node 0)
                   (outer (tree-node-left node))
                   (outer (tree-node-right node)))]))
  (outer root))

(define extree5 (tree-node 1
                           (tree-node -2
                                      (tree-node 1
                                                 (make-tree-node -1)
                                                 #f)
                                      (make-tree-node 3))
                           (tree-node -3
                                      (make-tree-node -2)
                                      #f)))


;; maybe if I just append a 1 whenever it finds one
;; then sum up at the end?
(define (path-sum root targetSum)
  (define hits '())
  (define (dfs node curr-sum)
    (if (not (tree-node? node))
        0
        (let ([inter-sum (+ curr-sum (tree-node-val node))])
          (when (= inter-sum targetSum)
            (set! hits (cons 1 hits)))
          (+ (dfs (tree-node-left node) (+ curr-sum (tree-node-val node)))
             (dfs (tree-node-right node) (+ curr-sum (tree-node-val node)))))))
  (define (outer node)
    (cond [(not (tree-node? node)) 0]
          [else (+ (dfs node 0)
                   (outer (tree-node-left node))
                   (outer (tree-node-right node)))]))
  (outer root)
  (apply + hits))

;; woohoo!
;; make it more rackety
(define (path-sum root targetSum)
  (define (dfs node curr-sum)
    (if (not (tree-node? node))
        '()
        (let ([inter-sum (+ curr-sum (tree-node-val node))])
          (append
           (if (= inter-sum targetSum) (list 1) '())
           (dfs (tree-node-left node) (+ curr-sum (tree-node-val node)))
           (dfs (tree-node-right node) (+ curr-sum (tree-node-val node)))))))

  (define (outer node)
    (cond [(not (tree-node? node)) '()]
          [else (append (dfs node 0)
                   (outer (tree-node-left node))
                   (outer (tree-node-right node)))]))

  (apply + (outer root)))

;; yes, it's more rackety but now it doesn't pass the most pathological of tests

(path-sum extree5 -1)
(path-sum extree 8)
(path-sum extree2 22)
(path-sum extree3 -5)
(path-sum extree4 -2)
(path-sum (make-tree-node 1) 0)
