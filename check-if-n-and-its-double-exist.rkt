#lang racket
(require racket)

;; idea
;; keep a copy of the input list
;; and a copy of the input list sorted
;; then binary search
;; let v = (first unsorted-input)
;; then if v // 2 in sorted-input, great, else go to (cadr unsorted-input)

(define-syntax (bsearch stx)
  (define xs (syntax->list stx))
  (datum->syntax stx
                 `(λ (ls target)
                    (define (ptr-narrow left right)
                      (let* ([mid (quotient (+ left right) 2)]
                             [mid-val (list-ref ls mid)])
                          (cond [(= mid-val target) ,(cadr xs)]
                                [(> left right) ,(caddr xs)]
                                [(> mid-val target) (ptr-narrow left (sub1 mid))]
                                [else (ptr-narrow (add1 mid) right)])))
                    (ptr-narrow 0 (sub1 (length ls))))))

(define (search ls target)
  (if (empty? ls) #f ((bsearch #t #f) ls target)))

(define (check-if-exist arr)
  (let rc ([sarr (sort arr <)])
    (cond [(empty? sarr) #f]
          [(search (rest sarr) (* (first sarr) 2)) #t]
          [(search (rest sarr) (/ (first sarr) 2)) #t]
          [else (rc (rest sarr))])))
