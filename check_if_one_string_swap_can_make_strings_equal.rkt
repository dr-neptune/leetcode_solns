#lang racket
(require racket)

(define (check-rest-for-letter strls1 strls2 char-2 char-1)
  (let rc ([indices (indexes-of strls1 char-2)])
    (cond [(empty? indices) #f]
          [(equal? strls2 (list-set strls1 (first indices) char-1)) #t]
          [else (rc (rest indices))])))

(define (union-str-strls str)
  (if (list? str) str (string->list str)))

(define (are-almost-equal s1 s2)
  (let* ([s1 (union-str-strls s1)]
         [s2 (union-str-strls s2)])
    (cond [(empty? s1) #t]
          [(char=? (first s1) (first s2)) (are-almost-equal (rest s1) (rest s2))]
          [else (check-rest-for-letter (rest s1) (rest s2) (first s2) (first s1))])))
