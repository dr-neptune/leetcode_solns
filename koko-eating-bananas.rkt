#lang racket
(require racket)

(define expiles '(3 6 7 11))
(define exh 8)

;; idea
;; get minimum amount of bananas that must be eaten in h hours
;; i.e. sum(piles) // 8

(quotient (foldl + 0 expiles) exh)

;; then check if it can be done with that value k
(quotient/remainder 11 3)

(foldl + 0 (map (λ (v) (let-values ([(q r) (quotient/remainder v 4)])
         (match r
           [0 q]
           [_ (add1 q)]))) expiles))

;; then iterate until you get the min val of k

(define expiles '(30 11 23 4 20))
(define exh 5)

(define expiles '(30 11 23 4 20))
(define exh 6)

(let ([piles expiles]
      [h exh])
  (define (check-if-possible k)
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v k)])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let loop ([min-bananas (quotient (foldl + 0 piles) h)])
    (if (check-if-possible min-bananas)
        min-bananas
        (loop (add1 min-bananas)))))

(define (min-eating-speed piles h)
  (define (check-if-possible k)
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v k)])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let loop ([min-bananas (quotient (foldl + 0 piles) h)])
    (if (check-if-possible min-bananas)
        min-bananas
        (loop (add1 min-bananas)))))

;; div by 0
(define expiles '(312884470))
(define exh 968709470)

(let ([piles expiles]
      [h exh])
  (define (check-if-possible k)
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v k)])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let loop ([min-bananas (quotient (foldl + 0 piles) h)])
    (if (check-if-possible min-bananas)
        min-bananas
        (loop (add1 min-bananas)))))
