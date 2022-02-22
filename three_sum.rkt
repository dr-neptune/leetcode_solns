#lang racket
(require racket)

;; idea
;; depth first search

;; get first 3 values
;; if they do not sum to 0, drop the last value and take the value after
;; continue until there are no elements in the list left
;; if there are no summations to 0, then take elements 2 3 4 and repeat above
;; if ls length is 3, then add the value if it sums to 0, else return results list

(define (zero-sum a b c) (= 0 (+ a b c)))

(define exls '(-1 0 1 2 -1 -4))

(apply zero-sum (take exls 3))

(take (rest exls) 3)

(apply zero-sum (take (rest exls) 3))

(take (rest exls) 2)

(take (remove 3 (rest exls)) 3)

;; take first value
;; take second value
;; check if rest of list contains a 3rd value that sums to 0
;; if found, append it
;;

;; (-1 0 1 2 -1 -4)
;; (-1) (0) (1 2 -1 -4) -> (-1 0 1)
;; (-1) (1) (2 -1 -4) -> none
;; (-1) (2) (-1 -4) -> (-1 2 -1)
;; (-1) (-1) (-4) -> none

;; (0) (1) (2 -1 -4) -> (0 1 -1)
;; (0) (2) (-1 -4) -> none
;; (0) (-1) (-4) -> none

;; (1) (2) (-1 -4) -> (1 2 -1)
;; (1) (-1) (-4) -> none

;; (2) (-1) (-4) -> none

;; return (dedupe ((-1 0 1) (-1 2 -1) (0 1 -1) (1 2 -1)))

(define (zero-sum-check a b c)
  (if (= 0 (+ a b c))
      (list a b c)
      '()))

(define (dfs ls [full-results '()])
  (if (= (length ls) 3)
      (append (apply zero-sum-check ls) full-results)
      (let try ([f (first ls)] [s (second ls)] [r (cddr ls)] [results '()])
        (if (empty? r)
            (dfs (rest ls) (append full-results results))
            (try f (first r) (rest r)
                 (append results (map (curry zero-sum-check f s) r)))))))

(define (element-sort ls)
  (cond [(empty? ls) '()]
        [(list? (first ls)) (remove-duplicates (map (λ (l) (sort l <)) ls))]
        [else (list (sort ls <))]))

(define (three-sum nums)
  (cond [(< (length nums) 3) '()]
        [else (element-sort (filter (compose not empty?) (dfs nums)))]))

(three-sum '(0 1 1))

(element-sort '((1 4 2) (2 4 1)))

(element-sort '(1 2 3))

(list? (first (dfs exls)))

(three-sum '(0 0 0 0))


(element-sort '((-1 0 1) (-1 2 -1) (0 1 -1)))

(remove-duplicates (map (λ (l) (sort l <)) '((-1 0 1) (-1 2 -1) (0 1 -1))))

(element-sort (filter (compose not empty?) (dfs '(0 0 0 0))))
(element-sort (filter (compose not empty?) (dfs exls)))

(element-sort (dfs exls))



(define (three-sum nums)
  (cond [(< (length nums) 3) '()]
        [(= 3 (length nums)) (element-sort nums)]
        [else (element-sort (filter (compose not empty?) (dfs nums)))]))


(element-sort (filter (compose not empty?) (dfs '(0 1 1))))

(three-sum exls)
(three-sum '())
(three-sum '(0))

(three-sum '(0 0 0))

(three-sum '(0 1 1))



(map add1 (cddr exls))

;; iter 1

(define f (first exls))
(define s (second exls))
(define r (cddr exls))

(apply append (map (curry zero-sum-check f s) r))

(apply append (map (lambda (x) (zero-sum-check f s x)) r))

;; iter 2
(define s (first r))
(define r (rest r))
(apply append (map (lambda (x) (zero-sum-check f s x)) r))

;; iter 3
(define s (first r))
(define r (rest r))
(apply append (map (lambda (x) (zero-sum-check f s x)) r))

;; iter 4
(define s (first r))
(define r (rest r))
(apply append (map (lambda (x) (zero-sum-check f s x)) r))

;; iter 5
(define s (first r))
(define r (rest r))
(apply append (map (lambda (x) (zero-sum-check f s x)) r))

;; recurse 1
(if (empty? r)
    (dfs (rest ls)))



(cddr exls)

(apply zero-sum-check '(1 0 -2))
