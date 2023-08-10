#lang racket
(require racket)

(define (check-dict value dict)
  (let ([found (hash-ref dict value #f)])
    (if (false? found)
        #f
        (begin
          (hash-update! dict value sub1)
          (when (< (hash-ref dict value) 0) #f)))))

(define (is-anagram s t)
  (if (= (string-length s)
         (string-length t))
      (let ([results (make-hash)])
        (begin
          (for-each (lambda (c) (hash-update! results c add1 0)) (string->list s))
          (for-each (lambda (c) (check-dict c results)) (string->list t))
          (apply = (cons 0 (hash-values results)))))
      #f))

(define exstr1 "anagram")
(define exstr2 "nagaram")
(is-anagram exstr1 exstr2)


;; try again
(define (is-anagram s t)
  (let ([sorted-chars (λ (s) (sort (string->list s) char<?))])
    (equal? (sorted-chars s) (sorted-chars t))))
