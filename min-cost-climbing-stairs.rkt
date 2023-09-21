#lang racket
(require racket)

;; idea
;; start from the back?
(define exls1 '(10 15 20))
(define exls '(1 100 1 1 1 100 1 1 100 1))

;; 1 step -> 1
;; 2 steps -> 1
;; 1 step -> 1
;; 2 steps -> 1
;; 2 steps -> 1
;; 2 steps -> 1
;; total cost: 6

(let ([cost exls])
  (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ..1)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a" candidates rest-costs))
         (loop rest-costs (cons (apply min candidates) paid)))]
      [(list a b) (cons (min a b) paid)])))

;; if we take the first, we must only drop 1
;; otherwise we must drop 2
(let ([cost exls])
  (apply + (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ..1)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a" candidates rest-costs))
         (match candidates
           [(list a b) #:when (> a b) (loop (append rest-costs (list a)) (cons b paid))]
           [(list a _) (loop rest-costs (cons a paid))]))]
      [(list a b) (cons (min a b) paid)]
      [(list a) paid]))))

(define (min-cost-climbing-stairs cost)
  (apply +
         (let loop ([costs cost]
                    [paid '()])
           (match costs
             [(list a b c ..1)
              (let-values ([(rest-costs candidates) (split-at-right costs 2)])
                ;; (displayln (format "candidates: ~a rest-costs: ~a" candidates rest-costs))
                (match candidates
                  [(list a b) #:when (> a b) (loop (append rest-costs (list a)) (cons b paid))]
                  [(list a _) (loop rest-costs (cons a paid))]))]
             [(list a b) (cons (min a b) paid)]
             [(list a) paid]))))

(define exls2 '(0 2 2 1))

(let ([cost exls2])
  (apply + (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ..1)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a paid: ~a" candidates rest-costs paid))
         (match candidates
           [(list a b) #:when (> a b) (loop (append rest-costs (list a)) (cons b paid))]
           [(list a _) (loop rest-costs (cons a paid))]))]
      [(list a b) (cons (min a b) paid)]
      [(list a) paid]))))

;; this is a greedy approach
;; but it is not globally optimal
