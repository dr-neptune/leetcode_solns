#lang racket
(require racket)

(define exarr '(#\a #\a #\b #\c #\c #\c))

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (compress chars)
  (length (flatten (hash->list (hash-table-counter chars)))))

(list-set)

(define (compress chars)
  (let ([vchars (list->vector chars)])
    (vector-set! vchars 1 #\n)
    vchars))

;; lame
