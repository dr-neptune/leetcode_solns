#lang racket
(require racket)

(define exspells '(5 1 3))
(define expotions '(1 2 3 4 5))
(define exsuccess 7)

(define exspells '(3 1 2))
(define expotions '(8 5 8))
(define exsuccess 16)

(define exspells '(15 8 19))
(define expotions '(38 36 23))
(define exsuccess 328)


;; (5 1 3)
;; 5
;; 5 * 3 = 15 yes
;; 5 * 2 = 10 yes
;; 5 * 1 = 5 no

;; base racket might be able to make use of partition here
;; might be too slow, as this probably needs binary search
( (partition (λ (n) (< (* 5 n) exsuccess)) expotions))

(let ([spells exspells]
      [potions expotions]
      [success exsuccess])
  (for/list ([spell exspells])
    (let-values ([(fail success) (partition (λ (potion) (< (* spell potion) success)) potions)])
      (length success))))

(define (successful-pairs spells potions success)
  (for/list ([spell spells])
    (let-values ([(fail success) (partition (λ (potion) (< (* spell potion) success)) potions)])
      (length success))))

;; probably fine, but time limit exceeded
;; let's do it again with binary search

(define (binary-search-partition pred ls)
  #:pass)

;; so we want to partition a sorted list using binary search
;; the end state is that we have (list a ... (pred fail) (pred success) ... z)
;; and we return (list (list a ... (pred fail)) (list (pred success) ... z))

(let ([ls '(1 2 3 4 5)]
      [pred (λ (n) (< (* n 5) 7))])
  (let ([vec (vector->list ls)])
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length vec))])
      (let* ([midpoint (quotient (+ lhs rhs) 2)]
             [element (vector-ref vec midpoint)])

        ))))

;; (1 2 3 4 5)

;; (1 2 3 4 5)
;; m: 4 + 0 / 2 = 2 = 3
;; 3 is true
;; so move the rhs inwards, rhs := midpoint
;; l: 0 r: 2 m: (+ 0 2) // 2 = 1
;; 2 is true
;; so rhs := midpoint
;; l: 0 r: 1 m: (+ 0 1) // 2 = 0
;; 1 is #f
;; check if the value to the right is true
;; it is, so return partitions
;; if it isn't, take values until it is
;; maybe I can get away without using LHS?

(let ([ls '(1 2 3 4 5)]
      [pred? (λ (n) (< (* n 5) 7))])
  (let ([vec (list->vector ls)])
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length vec))])
      (let* ([midpoint (quotient (+ lhs rhs) 2)]
             [middle-element (vector-ref vec midpoint)])
        (displayln (format "l: ~a r: ~a m: ~a e: ~a" lhs rhs midpoint middle-element))
        (if (pred? middle-element)
            (if (and (pred? middle-element)
                     (not (pred? (vector-ref vec (add1 midpoint)))))
                midpoint
                (loop midpoint rhs))
            (loop lhs midpoint))))))

(define (bs-partition pred ls)
  (define (get-splitpoint vec)
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length vec))])
      (let* ([midpoint (quotient (+ lhs rhs) 2)]
             [middle-element (vector-ref vec midpoint)])
        (displayln (format "l: ~a r: ~a m: ~a e: ~a" lhs rhs midpoint middle-element))
        (if (pred middle-element)
            (if (and (pred middle-element)
                     (not (pred (vector-ref vec (add1 midpoint)))))
                midpoint
                (loop midpoint rhs))
            (loop lhs midpoint)))))
  (let ([vec (list->vector (sort ls <))])
    (split-at ls (add1 (get-splitpoint vec)))
    ;; (let-values ([(lt gt) (split-at ls (add1 (get-splitpoint vec)))])
    ;;   (list lt gt))
    ))

(bs-partition (λ (n) (< (* n 5) 7)) '(1 2 3 4 5))
(bs-partition (λ (n) (< (* n 3) 16)) '(8 5 8))

(define (successful-pairs spells potions success)
  (for/list ([spell spells])
    (let-values ([(fail success) (bs-partition (λ (potion) (< (* potion spell) success)) potions)])
      (length success))))

(successful-pairs exspells expotions 7)

#|

l: 0 r: 4 m: 2 e: 3
l: 0 r: 2 m: 1 e: 2
l: 0 r: 1 m: 0 e: 1
l: 0 r: 4 m: 2 e: 3
l: 2 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4
l: 3 r: 4 m: 3 e: 4

|#

;; find the partition point such that predicate becomes false
;; idea
(let ([ls '(1 2 3 4 5)]
      [pred? (λ (n) (< (* n 3) 7))])
  (let ([vec (list->vector ls)])
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length vec))])
      (if (= lhs rhs)
          #f
      (let* ([midpoint (quotient (+ lhs rhs) 2)]
             [middle-element (vector-ref vec midpoint)])
        (displayln (format "l: ~a r: ~a m: ~a e: ~a" lhs rhs midpoint middle-element))
        (if (pred? middle-element)
            (if (and (pred? middle-element)
                     (not (pred? (vector-ref vec (add1 midpoint)))))
                midpoint
                (loop (add1 midpoint) rhs))
             (loop lhs (sub1 midpoint))))))))

(define (bs-partition pred ls)
  (define (get-splitpoint vec)
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length vec))])
      (if (>= lhs rhs)
          #f
          (let* ([midpoint (quotient (+ lhs rhs) 2)]
                 [middle-element (vector-ref vec midpoint)])
            (displayln (format "l: ~a r: ~a m: ~a e: ~a" lhs rhs midpoint middle-element))
            (if (pred middle-element)
                (if (and (pred middle-element)
                         (not (pred (vector-ref vec (add1 midpoint)))))
                    midpoint
                    (loop (add1 midpoint) rhs))
                (loop lhs (sub1 midpoint)))))))
  (let* ([vec (list->vector (sort ls <))]
         [split-point (get-splitpoint vec)])
    (if split-point
        (split-at ls (add1 split-point))
        0)))

(bs-partition (λ (n) (< (* n 3) 7)) '(1 2 3 4 5))
(bs-partition (λ (n) (< (* n 3) 16)) '(8 5 8))



(define (successful-pairs spells potions success)
  (for/list ([spell spells])
    (let-values ([(fail success) (bs-partition (λ (potion) (< (* potion spell) success)) potions)])
      (length success))))

(successful-pairs exspells expotions 7)

;; not there yet!

;; try again with the binary-partition
;; idea
;; we have a list
;; (1 2 3 4 5)
;; start with l: 0 r: 4 m: (l + r) // 2
;; if v[m] is true, then move the right point to (sub1 mid)
;; if v[m] is false,
;;   check if v[m + 1] is true
;;    if so,
;;      return m
;;    otherwise
;;      move the left point to (add1 mid)


(for/list ([j exspells])
  (let ([ls expotions]
        [pred? (λ (n) (>= (* n j) exsuccess))])
    (let ([vec (list->vector ls)])
      (let loop ([lhs 0]
                 [rhs (sub1 (vector-length vec))])
        (if (> lhs rhs)
            #f
            (let* ([midpoint (quotient (+ lhs rhs) 2)]
                   [middle-element (vector-ref vec midpoint)])
              (displayln (format "l: ~a r: ~a m: ~a e: ~a" lhs rhs midpoint middle-element))
              (match (pred? middle-element)
                [#t (cond [(not (pred? (vector-ref vec (sub1 midpoint)))) midpoint]
                          [else (loop lhs (sub1 rhs))])]
                [_ (loop (add1 midpoint) rhs)])))))))

(define (binary-partition pred ls)
  (define (get-breakpoint vec)
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length vec))])
      (if (> lhs rhs)
          (values (vector-length vec))
          (let* ([midpoint (quotient (+ lhs rhs) 2)]
                 [middle-element (vector-ref vec midpoint)])
            ;; (displayln (format "l: ~a r: ~a m: ~a e: ~a" lhs rhs midpoint middle-element))
            (match (pred middle-element)
              [#t (cond [(zero? midpoint) midpoint]
                        [(not (pred (vector-ref vec (sub1 midpoint)))) midpoint]
                        [else (loop lhs (sub1 rhs))])]
              [_ (loop (add1 midpoint) rhs)])))))
  (let ([vec (list->vector (sort ls <))])
    (split-at ls (get-breakpoint vec))))

(define (successful-pairs spells potions success)
  (for/list ([spell spells])
  (let-values ([(_ success) (binary-partition (λ (n) (>= (* n spell) success)) potions)])
    (length success))))

(for/list ([spell exspells])
  (let-values ([(_ success) (binary-partition (λ (n) (>= (* n spell) exsuccess)) expotions)])
    (length success)))


;; try solutions solution
;; above is too slow with vector
