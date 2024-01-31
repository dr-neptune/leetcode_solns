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


(define extree (tree-node 4
                          (tree-node 2
                                     (make-tree-node 1)
                                     (make-tree-node 3))
                          (make-tree-node 5)))


#|

idea

traverse the binary search tree

at each step, compare the value of a node with the target value
if node-val lhs increases the deviation, don't go there
if node-val rhs increasaes the deviation, don't go there

|#

(define extree (tree-node 4
                          (tree-node 2
                                     (make-tree-node 1)
                                     (make-tree-node 3))
                          (make-tree-node 5)))

(define extarget 3.5)

(let ([root extree]
      [target extarget])
  (let ([min-dev (cons +Inf.0 0)])
    (let loop ([root root])
      (displayln (format "~a ~a" min-dev root))
      (match root
        [#f '()]
        [(tree-node v l r)
         (let ([dev (abs (- target v))])
           (displayln (format "dev: ~a\tmin-dev: ~a" dev min-dev))
           (when (< dev (car min-dev)) (set! min-dev (cons dev v)))
           (when (= dev (car min-dev)) (set! min-dev (cons dev (min v (cdr min-dev)))))
           (begin (loop l) (loop r)))]))
    (cdr min-dev)))


(define (closest-value root target)
  (let ([min-dev (cons +Inf.0 0)])
    (let loop ([root root])
      (match root
        [#f '()]
        [(tree-node v l r)
         (let ([dev (abs (- target v))])
           (when (< dev (car min-dev)) (set! min-dev (cons dev v)))
           (when (= dev (car min-dev)) (set! min-dev (cons dev (min v (cdr min-dev)))))
           (begin (loop l) (loop r)))]))
    (cdr min-dev)))


;; try again with binary search

(let ([root extree]
      [target extarget])
  (let* ([min-dev (cons +Inf.0 0)]
         [check-dev (λ (v)
                      (let ([dev (abs (- target v))])
                        (when (< dev (car min-dev)) (set! min-dev (cons dev v)))
                        (when (= dev (car min-dev)) (set! min-dev (cons dev (min v (cdr min-dev)))))))])
    (let loop ([root root])
      (displayln (format "~a ~a" min-dev root))
      (match root
        [#f '()]
        [(tree-node v l r)
         (cond [(> v target) (begin (check-dev v) (loop l))]
               [(< v target) (begin (check-dev v) (loop r))]
               [else v])]))
    (cdr min-dev)))


(define (closest-value root target)
  (let* ([min-dev (cons +Inf.0 0)]
         [check-dev (λ (v)
                      (let ([dev (abs (- target v))])
                        (cond [(< dev (car min-dev)) (set! min-dev (cons dev v))]
                              [(= dev (car min-dev)) (set! min-dev (cons dev (min v (cdr min-dev))))])))])
    (let loop ([root root])
      (if (false? root)
          '()
          (match-let ([(tree-node v l r) root])
            (cond [(> v target) (begin (check-dev v) (loop l))]
                  [(< v target) (begin (check-dev v) (loop r))]
                  [else (set! min-dev (cons 0 v))]))))
    (cdr min-dev)))
