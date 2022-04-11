#lang racket
(require racket)

(define (search nums target)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (list-ref nums pivot)])
      (cond [(> left right) #f]
            [(= pivot-val target) #t]
            [(< target pivot-val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
  (ptr-narrow 0 (sub1 (length nums))))

;; naive
(define (search-matrix matrix target)
  (search (flatten matrix) target))


;; idea
;; do a binary search to see what "bin" the number belongs in
;; then do a binary search for that row to see if it exists
(define exls '((1 3 5 7)(10 11 16 20)(23 30 34 60)))

;; get first elements
(define firsts (map first exls))

;; do a binary search to see what bucket it should be in
(define (check-bin ls target)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref ls mid)])
      (cond [(>= left right) (if (> mid-val target)
                                 (sub1 mid)
                                 mid)]
            [(> mid-val target) (ptr-narrow left (sub1 mid))]
            [else (ptr-narrow (add1 mid) right)])))
  (ptr-narrow 0 (sub1 (length ls))))

(check-bin firsts 15)
(list-ref firsts (check-bin firsts 1))

(list-ref exls (check-bin firsts 3))

;; now do a binary search on the row to see if its there
(search (list-ref exls (check-bin firsts 3)) 3)

(define (search-matrix matrix target)
  (search (list-ref matrix (check-bin (map first matrix) target)) target))

(search-matrix exls 23)

;; clean it up
(define (search nums target)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (list-ref nums pivot)])
      (cond [(> left right) #f]
            [(= pivot-val target) #t]
            [(< target pivot-val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
  (ptr-narrow 0 (sub1 (length nums))))

(define (check-bin ls target)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref ls mid)])
      (cond [(>= left right) (if (> mid-val target)
                                 (sub1 mid)
                                 mid)]
            [(> mid-val target) (ptr-narrow left (sub1 mid))]
            [else (ptr-narrow (add1 mid) right)])))
  (ptr-narrow 0 (sub1 (length ls))))

(define (search-matrix matrix target)
  (search (list-ref matrix (check-bin (map first matrix) target)) target))

(search-matrix '((1)) 0)
