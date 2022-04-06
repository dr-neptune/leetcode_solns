#lang racket
(require racket)

;; idea
;; get midpoint
;; if target > midpoint, return midpoint -> end
;; if target == midpoint, return index
;; if target < midpoint, return beginning -> midpoint

(define (search nums target [index (quotient (length nums) 2)])
  (let* ([midpoint (quotient (length nums) 2)]
         [midpoint-value (list-ref nums midpoint)]
         [index-val midpoint])
    (cond [(= target midpoint-value) index]
          [(> target midpoint-value) (search (take-right nums midpoint) target (+ index (sub1 index-val)))]
          [else (search (take nums midpoint) target (- index (add1 index-val)))])))

(search '(-1 0 3 5 9 12) 9)

(define exls '(-1 0 3 5 9 12))

;; get midpoint
(quotient (length exls) 2)

;; get value at midpoint
(list-ref exls 3)                       ; 3

;; value is < target, so return RHS of list
(take-right exls (sub1 3))
(define exls2 (take-right exls (sub1 3)))

;; get new midpoint
(quotient (length exls2) 2)             ; 3 + (add1 1) = 5
(list-ref exls2 1)

;; value is > target, so return LHS of list
(take exls2 1)
(define exls3 (take exls2 1))

;; get new midpoint
(quotient (length exls3) 2)             ; 5 - (add1 0) = 4


(list-ref exls3 0)

;; target = midpoint-value

(define (search nums target [index (quotient (length nums) 2)])
  (if (empty? nums)
      -1
      (let* ([midpoint-idx (quotient (length nums) 2)]
             [midpoint-val (list-ref nums midpoint-idx)])
        (cond [(= target midpoint-val) (sub1 index)]
              [(> target midpoint-val)
               (search (take-right nums (sub1 midpoint-idx)) target (+ index (add1 midpoint-idx)))]
              [else (search (take nums midpoint-idx) target (- index (add1 midpoint-idx)))]))))


(search exls 9)

(search exls 2)


;; get midpoint
(quotient (length exls) 2)

;; get value at midpoint
(list-ref exls 3)                       ; 3

;; target < 5, so we take the LHS
(define exls2 (take exls 3))

;; get midpoint
(quotient (length exls2) 2)
(list-ref exls2 1)

;; target > 0, so we take the RHS
(take-right exls2 1)

;; official solution
(define (search nums target)
  (let rc ([left 0]
           [right (sub1 (length nums))])
    (let* ([pivot (quotient (+ left (- right left)) 2)]
           [pivot-number (list-ref nums pivot)])
      (cond [(> left right) -1]
            [(= target pivot-number) (begin
                                       (println (format "l: ~a r: ~a p: ~a p-n: ~a" left right pivot pivot-number))
                                       pivot)]
            [(< target pivot-number) (begin
                                       (println (format "l: ~a r: ~a p: ~a p-n: ~a" left right pivot pivot-number))
                                       (rc left (sub1 pivot)))]
            [else (rc (add1 pivot) right)]))))


(define exls '(-1 0 3 5 9 12))

(search exls 9)


(+ left (quotient (- right left) 2))

(define (search nums target)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (list-ref nums pivot)])
      (cond [(> left right) -1]
            [(= pivot-val target) pivot]
            [(< target pivot-val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
  (if (> (length nums) 1)
      (ptr-narrow 0 (sub1 (length nums)))
      (ptr-narrow 0 (length nums))))

(search exls 13)
(search exls 9)
(search '(5) 5)
(search '(2 5) 5)
(search '(-1) 2)
