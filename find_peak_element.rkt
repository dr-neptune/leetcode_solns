#lang racket
(require racket)

;; idea
;; binary search
;; start at midpoint
;; if lhs and rhs < ele return
;; else recurse both halfway through beginning and halfway through end
;; must handle first and last values
;; maybe append elements to front and back to guarantee there is always an element?
;; and we have to return the index!

(define exnums '(1 2 3 1))
(define exnums '(1 2 1 3 5 6 4))
(define exnums '(2 1 1 1 1 1))
(define exnums '(8 7 6 5 4 3 2 1))
(define exnums '(1))
(define exnums '(1 2))
(define exnums '(3 4 3 2 1))


(let ([nums exnums])
  (let loop ([lhs 0]
             [rhs (sub1 (length nums))])
    (let ([mid (quotient (+ lhs rhs) 2)])
      (displayln (format "lhs: ~a mid: ~a rhs: ~a" lhs mid rhs))
      (cond
        [(> lhs rhs) -1]
            [(zero? mid) (if (> (list-ref nums mid)
                                  (list-ref nums 1))
                           0
                           -1)]
            [(= mid (sub1 (length nums)))
             (if (> (list-ref nums mid)
                    (list-ref nums (sub1 mid)))
                 mid
                 -1)]
            [else (let ([mid-ele (list-ref nums mid)]
                        [lh-mid (list-ref nums (sub1 mid))]
                        [rh-mid (list-ref nums (add1 mid))])
                    (cond [(and (< lh-mid mid-ele)
                                (< rh-mid mid-ele))
                           mid]
                          [else
                           (max (loop lhs (sub1 mid))
                                (loop (add1 mid) rhs))]))]))))

;; forgot the case where list < 3 elements

(let ([nums exnums])
  (match nums
    [(list a) 0]
    [(list a b) (if (> a b) 0 1)]
    [(list a b c d ...)
     (let loop ([lhs 0]
                [rhs (sub1 (length nums))])
       (let ([mid (quotient (+ lhs rhs) 2)])
         (displayln (format "lhs: ~a mid: ~a rhs: ~a" lhs mid rhs))
         (cond
           [(> lhs rhs) -1]
           [(zero? mid) (if (> (list-ref nums mid)
                               (list-ref nums 1)) 0 -1)]
           [(= mid (sub1 (length nums))) (if (> (list-ref nums mid)
                                                (list-ref nums (sub1 mid))) mid -1)]
           [else (let ([mid-ele (list-ref nums mid)]
                       [lh-mid (list-ref nums (sub1 mid))]
                       [rh-mid (list-ref nums (add1 mid))])
                   (cond [(and (< lh-mid mid-ele)
                               (< rh-mid mid-ele))
                          mid]
                         [else
                          (cons (loop lhs (sub1 mid))
                                (loop (add1 mid) rhs))]))])))]))

;; idea
;; we need to cover the list with binary search
;; and apply a predicate at each step
;; this is general enough to warrant it's own function
(define (bsearchf-indexes pred ls)
  (define (get-preds [lhs 0] [rhs (sub1 (length ls))])
    (let ([mid (quotient (+ lhs rhs) 2)])
      (displayln (format "lhs: ~a rhs: ~a mid: ~a" lhs mid rhs))
      (cond [(> lhs rhs) #f]
            [(pred (list-ref ls mid)) mid]
            [else (cons (get-preds lhs (sub1 rhs))
                        (get-preds (add1 lhs) rhs))])))
  (remove-duplicates (filter number? (flatten (get-preds)))))

;; we also need to break when we get a result
;; otherwise we have O(n) again :/

(bsearchf-indexes even? '(1 2 3 4 5))

(bsearchf-indexes (curry = 2) '(1 2 3 4 5))

(bsearchf-indexes '(1 2 3 4 5))

;; can we adjust the above to count for 3-tuples of numbers?

(define (bsearchf-indexes pred ls)
  (define (get-preds [lhs 0] [rhs (sub1 (length ls))])
    (let ([mid (quotient (+ lhs rhs) 2)])
      (displayln (format "lhs: ~a rhs: ~a mid: ~a" lhs mid rhs))
      (cond [(or (> mid (length ls))
                 (< mid 0)) -1]
        [(> lhs rhs) #f]
        [(pred (list-ref ls (sub1 mid))
               (list-ref ls mid)
               (list-ref ls (add1 mid))) mid]
        [else (cons (get-preds lhs (sub1 rhs))
                    (get-preds (add1 lhs) rhs))])))
  (remove-duplicates (filter number? (flatten (get-preds)))))

(bsearchf-indexes (λ (l m r) (and (< l m) (< r m))) '(1 2 1 4 5))

;; with soln
(let ([nums exnums])
  (let loop ([lhs 0]
             [rhs (sub1 (length nums))])
    (cond [(>= lhs rhs) rhs]
          [else
           (let ([mid (quotient (+ lhs rhs) 2)])
             (displayln (format "lhs: ~a mid: ~a rhs: ~a" lhs mid rhs))
             (if (> (list-ref nums mid)
                    (list-ref nums (add1 mid)))
                 (loop lhs mid)
                 (loop (add1 mid) rhs)))])))

(define (find-peak-element nums)
  (let loop ([lhs 0]
             [rhs (sub1 (length nums))])
    (cond [(>= lhs rhs) rhs]
          [else
           (let ([mid (quotient (+ lhs rhs) 2)])
             (if (> (list-ref nums mid)
                    (list-ref nums (add1 mid)))
                 (loop lhs mid)
                 (loop (add1 mid) rhs)))])))


(define (find-peak-element nums)
  (let ([nums (list->vector nums)])
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length nums))])
    (cond [(>= lhs rhs) rhs]
          [else
           (let ([mid (quotient (+ lhs rhs) 2)])
             (if (> (vector-ref nums mid)
                    (vector-ref nums (add1 mid)))
                 (loop lhs mid)
                 (loop (add1 mid) rhs)))]))))


(define (find-peak-element nums)
  (let ([nums (list->vector nums)])
    (let loop ([lhs 0]
               [rhs (sub1 (vector-length nums))])
      (match (>= lhs rhs)
        [#t rhs]
        [_ (let ([mid (quotient (+ lhs rhs) 2)])
             (if (> (vector-ref nums mid)
                    (vector-ref nums (add1 mid)))
                 (loop lhs mid)
                 (loop (add1 mid) rhs)))]))))
