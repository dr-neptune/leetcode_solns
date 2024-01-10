#lang racket
(require racket)

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (lambda (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (can-permute-palindrome s)
  (let* ([strls (string->list s)]
         [hsh (hash-table-counter strls)])
    (match (hash-keys hsh)
      [(list _) #t]
      [_ (match-let ([(list 1-mults _)
                      (call-with-values (λ () (partition odd? (hash-values hsh))) list)])
           (< (length 1-mults) 2))])))
