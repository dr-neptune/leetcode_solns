#lang racket
(require racket)

(define (make-translator str [null-val #\space])
  (let* ([strls (string->list str)]
         [hsh (make-hash (map cons strls (inclusive-range 1 (length strls))))])
      (hash-set! hsh null-val 0)
      hsh))

(define (translate-str str cipher)
  (let ([digit-list->int (λ (ls) (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))])
    (digit-list->int (map (λ (c) (hash-ref cipher c)) str))))

(define (order-comparator order)
  (let ([translator (make-translator order)])
    (λ (a b)
      (let* ([max-len (apply max (map string-length (list a b)))]
             [a (string->list (~a a #:min-width max-len))]
             [b (string->list (~a b #:min-width max-len))]
             [translate (curryr translate-str translator)])
        (< (translate a) (translate b))))))

(define (is-alien-sorted words order)
  (equal? (sort words (order-comparator order)) words))

(is-alien-sorted '("hello" "leetcode") "hlabcdefgijkmnopqrstuvwxyz")
