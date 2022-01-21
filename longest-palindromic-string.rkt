#lang racket
(require racket)

(define exstr1 "babad")  ;; bab
(define exstr2 "cbbd")   ;; bb
(define exstr3 "aacabdkacaa") ;; aca


(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))


(define (all-slides ls [k 0] [vals '()])
  (if (= k (length ls))
      vals
      (all-slides ls (add1 k) (append vals (sliding-window ls (add1 k))))))


(define (is-palindrome ls)
  (equal? ls (reverse ls)))


(define (longest-palindrome s)
  (let ([largest '(0 0)])
    (for ([val (in-list (all-slides (string->list s)))])
      (when (is-palindrome val)
        (when (> (length val) (first largest))
          (set! largest (list (length val) val)))))
    (list->string (second largest))))


(longest-palindrome exstr1)
(longest-palindrome exstr2)
(longest-palindrome exstr3)
