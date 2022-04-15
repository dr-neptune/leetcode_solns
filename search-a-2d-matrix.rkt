#lang racket
(require racket)

;; idea
;; do a binary search to see what "bin" the number belongs in
;; then do a binary search for that row to see if it exists
(define exls '((1 3 5 7)(10 11 16 20)(23 30 34 60)))

;; with a macro!
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



(define (lref ls ind)
  (if (= ind -1) '() (list-ref ls ind)))

(define (check-bin ls target)
  ((bsearch mid (if (> mid-val target) (sub1 mid) mid)) ls target))

(define (search ls target)
  (if (empty? ls) #f ((bsearch #t #f) ls target)))

(define (search-matrix matrix target)
  (search (lref matrix (check-bin (map first matrix) target)) target))

(search-matrix exls 1)
