#lang racket
(require racket)

;; binary search
(define (is-perfect-square num)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [p-sq (* pivot pivot)])
      (cond [(< right left) #f]
            [(= p-sq num) #t]
            [(< p-sq num) (ptr-narrow (add1 pivot) right)]
            [else (ptr-narrow left (sub1 pivot))])))
  (ptr-narrow 0 (sub1 (expt 2 31))))

;; tighter bounds
(define (is-perfect-square num)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [p-sq (* pivot pivot)])
      (cond [(< right left) #f]
            [(= p-sq num) #t]
            [(< p-sq num) (ptr-narrow (add1 pivot) right)]
            [else (ptr-narrow left (sub1 pivot))])))
  (if (< num 2) #t
      (ptr-narrow 2 (quotient num 2))))

;; newton's method
(define (is-perfect-square num)
  (if (< num 2) #t
      (let rc ([x (quotient num 2)])
        (if (> (* x x) num)
            (rc (quotient (+ x (quotient num x)) 2))
            (= (* x x) num)))))

(is-perfect-square 32)
