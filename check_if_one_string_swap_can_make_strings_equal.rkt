#lang racket
(require racket)

;; given two words
;; we have a valid single string swap if
;; traverse both strings as str ls
;; if same, continue on both
;; if different continue but add 1 to counter
;; if at the end of the string the difference count is 2, then #t else #f

(define (swap-count sls1 sls2 [num-diffs 0])
  (cond [(empty? sls1) (quotient num-diffs 2)]
        [(equal? (first sls1) (first sls2)) (swap-count (rest sls1) (rest sls2) num-diffs)]
        [(swap-count (rest sls1) (rest sls2) (add1 num-diffs))]))

(define (are-almost-equal s1 s2)
  (let ([s1 (string->list s1)] [s2 (string->list s2)])
    (< 2 (swap-count s1 s2))))

(define exstr1 (string->list "bank"))
(define exstr2 (string->list "kanb"))

(define (swap-count sls1 sls2 [num-diffs 0])
  (cond [(empty? sls1) (quotient num-diffs 2)]
        [(equal? (first sls1) (first sls2)) (swap-count (rest sls1) (rest sls2) num-diffs)]
        [(swap-count (rest sls1) (rest sls2) (add1 num-diffs))]))

(swap-count exstr1 exstr2)
