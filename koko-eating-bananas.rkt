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
    ;; (displayln (format "h: ~a piles: ~a k: ~a" h piles k))
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v (max k 1))])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let loop ([min-bananas (quotient (foldl + 0 piles) h)])
    (if (check-if-possible min-bananas)
        (max min-bananas 1)
        (loop (add1 min-bananas)))))


(define (min-eating-speed piles h)
  (define (check-if-possible k)
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v (max k 1))])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let loop ([min-bananas (quotient (foldl + 0 piles) h)])
    (if (check-if-possible min-bananas)
        (max min-bananas 1)
        (loop (add1 min-bananas)))))


(define expiles '(1000000000 1000000000))
(define exh 3)

(let ([piles expiles]
      [h exh])
  (define (check-if-possible k)
    (displayln (format "h: ~a piles: ~a k: ~a" h piles k))
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v (max k 1))])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let loop ([min-bananas (quotient (foldl + 0 piles) h)]
             [max-bananas (apply max piles)])
    (if (check-if-possible min-bananas)
        (max min-bananas 1)
        (min (loop (add1 min-bananas) max-bananas)
             (loop min-bananas (sub1 max-bananas))))))

;; idea
;; we have k in some boundary
;; between min-k and max (piles)
;; we can binary search to find k such that check-if-possible is true for m and false for m-1



(define (bsearchf-indexes pred ls)
  (define (get-preds [lhs 0] [rhs (sub1 (length ls))])
    (let ([mid (quotient (+ lhs rhs) 2)])
      (displayln (format "lhs: ~a rhs: ~a mid: ~a" lhs mid rhs))
      (cond [(> lhs rhs) #f]
            [(pred (list-ref ls mid)) mid]
            [else (cons (get-preds lhs (sub1 rhs))
                        (get-preds (add1 lhs) rhs))])))
  (remove-duplicates (filter number? (flatten (get-preds)))))


(let ([piles expiles]
      [h exh])
  (define (check-if-possible k)
    (displayln (format "h: ~a piles: ~a k: ~a" h piles k))
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v (max k 1))])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let bsearch ([lhs (quotient (foldl + 0 piles) h)]
                [rhs (apply max piles)])
    (let ([mid (quotient (+ lhs rhs) 2)])
      (displayln (format "lhs ~a rhs ~a" lhs rhs))
      (cond [(and (check-if-possible mid)
                  (not (check-if-possible (sub1 mid)))) mid]
            [(>= lhs rhs) (apply max piles)]
            [else
             (min
               (bsearch (add1 mid) rhs)
               (bsearch lhs (sub1 mid)))]))))


;; translate from python
(let ([piles expiles]
      [h exh])
  (let bsearch ([lhs 1]
                [rhs (apply max piles)])
    (cond [(>= lhs rhs) lhs]
          [else
           (let* ([mid (quotient (+ lhs rhs) 2)]
                  [hours (foldl + 0 (map (λ (v) (ceiling (/ v mid))) piles))])
             (if (> hours h)
                 (bsearch (add1 mid) rhs)
                 (bsearch lhs mid)))])))


(define (min-eating-speed piles h)
  (let bsearch ([lhs 1]
                [rhs (apply max piles)])
    (cond [(>= lhs rhs) lhs]
          [else
           (let* ([mid (quotient (+ lhs rhs) 2)]
                  [hours (foldl + 0 (map (λ (v) (ceiling (/ v mid))) piles))])
             (if (> hours h)
                 (bsearch (add1 mid) rhs)
                 (bsearch lhs mid)))])))


(let ([piles expiles]
      [h exh])
  (define (check-if-possible k)
    (displayln (format "h: ~a piles: ~a k: ~a" h piles k))
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v (max k 1))])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let bsearch ([lhs 1] [rhs (apply max piles)])
    (let ([mid (quotient (+ lhs rhs) 2)])
      (cond [(>= lhs rhs) lhs]
            [(check-if-possible mid) (bsearch lhs mid)]
            [else (bsearch (add1 mid) rhs)]))))


(define (min-eating-speed piles h)
  (define (check-if-possible k)
    (<= (foldl + 0
               (map (λ (v) (let-values ([(q r) (quotient/remainder v (max k 1))])
                             (match r
                               [0 q]
                               [_ (add1 q)])))
                    piles))
        h))
  (let bsearch ([lhs 1] [rhs (apply max piles)])
    (let ([mid (quotient (+ lhs rhs) 2)])
      (cond [(>= lhs rhs) lhs]
            [(check-if-possible mid) (bsearch lhs mid)]
            [else (bsearch (add1 mid) rhs)]))))
