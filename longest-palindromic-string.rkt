#lang racket
(require racket)

(define exstr1 "babad")  ;; bab
(define exstr2 "cbbd")   ;; bb

;; idea
;; brute force
;; get every combination of the letters and return only those that are palindromes
;; then find the longest one

(map string (combinations (string->list exstr1)))

;; idea
;; stream the combinations
;; check each for palindrome
;; keep a max size palindrome

(define (is-palindrome ls)
  (equal? ls (reverse ls)))

(define largest '(0 0))

(for ([val (in-combinations (string->list exstr1))])
  (when (is-palindrome val)
    (when (> (length val) (first largest))
      (set! largest (list (length val) val))))
  (println val))


(define (longest-palindrome s)
  (let ([largest '(0 0)])
    (for ([val (in-permutations (string->list s))])
      (when (is-palindrome val)
        (when (> (length val) (first largest))
          (set! largest (list (length val) val)))))
    (list->string (second largest))))

(longest-palindrome exstr1)

(longest-palindrome exstr2)


(define exstr3 "aacabdkacaa")

(longest-palindrome exstr3)


;; in-list coerces the list to a stream for use with a for statement
(in-list '(1 2 3))

;; write a function that gets a sliding window of every size over the string
;; string
;; strin tring
;; stri trin ring
;; str tri rin ing
;; st tr ri in ng
;; s t r i n g
