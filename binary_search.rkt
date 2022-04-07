#lang racket
(require racket)

(define (search nums target)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (list-ref nums pivot)])
      (cond [(> left right) -1]
            [(= pivot-val target) pivot]
            [(< target pivot-val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
  (ptr-narrow 0 (sub1 (length nums))))

(search exls 13)       ;; -1
(search exls 9)        ;; 4
(search '(5) 5)        ;; 0
(search '(2 5) 5)      ;; 1
(search '(-1) 2)       ;; -1
