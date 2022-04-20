#lang racket
(require racket)

;; two pointers
(define (two-sum numbers target [l 0] [r (sub1 (length numbers))])
  (if (< l r)
      (let ([s (+ (list-ref numbers l) (list-ref numbers r))])
        (cond [(= s target) (map add1 (list l r))]
              [(< s target) (two-sum numbers target (add1 l) r)]
              [else (two-sum numbers target l (sub1 r))])) #f))

(two-sum '(2 7 11 15) 9)
(two-sum '(2 3 4) 6)
(two-sum '(-1 0) -1)

;; idea
;; binary search
;; grab the first item
;; then binary search the rest of the list to see if any of them equal target - curr
;; if so, return #t, i, mid
;; if #f, move on to the next item in the list
(define (two-sum numbers target)
  #:pass)

(define (bsearch ls target)
  (let rc ([left 0] [right (sub1 (length ls))])
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref ls mid)])
      (cond [(> left right) #f]
            [(= target mid-val) mid]
            [(> target mid-val) (rc (add1 mid) right)]
            [else (rc left (sub1 mid))]))))

(define exls '(2 7 11 15))

;; trace it out
;; take first item
(first exls)

;; search rest of the list for an item which fits
(define (get-ind ls proc)
  (cond [(empty? ls) 0]
        [(proc (first ls)) 0]
        [else (add1 (get-ind (rest ls) proc))]))

(get-ind (rest exls) (λ (v) (= v (- 9 (first exls)))))

;; now we need to account for the change in list length
(bsearch (rest exls) 9)

(define (bsearch ls target)
  (let rc ([left 0] [right (sub1 (length ls))])
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref ls mid)])
      (cond [(> left right) #f]
            [(= target mid-val) mid]
            [(> target mid-val) (rc (add1 mid) right)]
            [else (rc left (sub1 mid))]))))

(define (two-sum numbers target)
  (let rc ([ls numbers]
           [ind 1])
    (let* ([fval (first ls)]
           [bsearch-results (bsearch (rest ls) (- target fval))])
      (if bsearch-results
          (list ind (+ (add1 ind) bsearch-results))
          (rc (rest ls) (add1 ind))))))

(two-sum exls 9)

(two-sum '(2 3 4) 6)
(two-sum '(-1 0) -1)

;; using vector
(define (bsearch vec target)
  (let rc ([left 0] [right (sub1 (vector-length vec))])
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (vector-ref vec mid)])
      (cond [(> left right) #f]
            [(= target mid-val) mid]
            [(> target mid-val) (rc (add1 mid) right)]
            [else (rc left (sub1 mid))]))))

(define (two-sum numbers target)
  (let* ([ls (list->vector numbers)]
         [first (λ (ls) (vector-ref ls 1))]
         [rest (λ (ls) (vector-drop ls 1))])
    (let rc ([ls ls]
             [ind 1])
      (let* ([fval (first ls)]
             [bsearch-results (bsearch (rest ls) (- target fval))])
        (if bsearch-results
            (list ind (+ (add1 ind) bsearch-results))
            (rc (rest ls) (add1 ind)))))))

(vector-ref (list->vector '(1 2 3)) 1)
