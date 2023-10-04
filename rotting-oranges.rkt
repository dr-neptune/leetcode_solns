#lang racket
(require racket)

(define exgrid '((2 1 1)
                 (1 1 0)
                 (0 1 1)))

;; idea
;; check if anything isn't 4 directionally linked to another
;; if so, return -1
;; otherwise for each rotten orange, rot each directionally linked non-rotten orange
;; then check if all oranges are rotten
;; if so, return num-minutes
;; if not, increment num-minutes

;; better yet
;; start by checking globally
;; if no unlinked oranges then
;; scan for a rotten orange
;; start at a rotten orange

;; get xs
;; get ys

;;

(define exgrid '((2 1 1)
                 (1 1 0)
                 (0 1 1)))

(define (grid->vec grid)
  (vector-map list->vector (list->vector grid)))

(define (get-element grid x y)
  (cond [(or (negative? x)
             (negative? y)
             (> x (sub1 (vector-length (vector-ref grid 0))))
             (> y (sub1 (vector-length grid)))) -1]
        [else
         (let ([y-row (vector-ref grid y)])
           (vector-ref y-row x))]))

(let ([grid exgrid])
  (get-element (grid->vec grid) 0 0))

;; now we want to get all adjacent elements
(define (get-adjacent grid x y)
  (filter (compose not negative?)
          (map (curry get-element grid)
               `((,(add1 x) y)
                 (,(sub1 x) y)
                 (x ,(add1 y))
                 (x ,(sub1 y))))))

;; rework this to go clockwise starting from midnight
;; or use an alist to tag them
(let ([x 1] [y 1])
  (filter (compose not negative?)
          (map (λ (ls) (apply (curry get-element (grid->vec exgrid)) ls))
               (list (list (add1 x) y)
                 (list (sub1 x) y)
                 (list x (add1 y))
                 (list x (sub1 y))))))
