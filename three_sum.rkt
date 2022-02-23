#lang racket
(require racket)

;; based on DFS
(define (zero-sum-check a b c)
  (if (= 0 (+ a b c))
      (list a b c)
      '()))

(define (dfs ls [full-results '()])
  (if (= (length ls) 3)
      (append (list (apply zero-sum-check ls)) full-results)
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

;; based on 2-sum
(define (element-sort ls)
  (cond [(empty? ls) '()]
        [(list? (first ls)) (remove-duplicates (map (λ (l) (sort l <)) ls))]
        [else (list (sort ls <))]))

(define (get-vals nums target)
  (if (empty? nums) '()
      (let ([check (filter (lambda (n) (= (first nums) (- target n)))
                           (rest nums))])
        (if (empty? check)
            (get-vals (rest nums) target)
            (list (first nums) (first check))))))

(define (get-three-sums nums [results '()] [original-nums nums])
  (if (empty? nums)
      results
      (let ([duo (get-vals (remove (first nums) original-nums) (- (first nums)))])
        (if (empty? duo)
            (get-three-sums (rest nums) results original-nums)
            (get-three-sums (rest nums)
                            (append results (list (append (list (first nums)) duo)))
                            original-nums)))))

(define (three-sum nums)
  (element-sort (filter (compose not empty?) (get-three-sums nums))))

(define (three-sum nums)
  (filter (compose not empty?) (get-three-sums nums)))

;; above is not quite right
(define exls '(-1 0 1 2 -1 -4 -2 -3 3 0 4))
(three-sum '(-1 0 1 2 -1 -4 -2 -3 3 0 4))
(element-sort (filter (compose not empty?) (get-three-sums exls)))

(three-sum '(-1 0 1 2 -1 -4))
