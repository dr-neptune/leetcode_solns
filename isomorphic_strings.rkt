#lang racket
(require racket)

(define (is-isomorphic s t)
  (define (check-mapping str-ls1 str-ls2)
    (let ([letter-map (for/hash ([s1 str-ls1] [s2 str-ls2]) (values s1 s2))])
      (for/and ([s1 str-ls1] [s2 str-ls2])
        (equal? (hash-ref letter-map s1) s2))))
  (match-let ([(list str1 str2) (map string->list (list s t))])
    (andmap identity (list (check-mapping str1 str2) (check-mapping str2 str1)))))

(define (is-isomorphic s t)
  (define (check-mapping str-ls1 str-ls2)
    (let ([letter-map (for/hash ([s1 str-ls1] [s2 str-ls2]) (values s1 s2))])
      (for/and ([s1 str-ls1] [s2 str-ls2])
        (equal? (hash-ref letter-map s1) s2))))
  (match-let ([(list str1 str2) (map string->list (list s t))])
    (and (check-mapping str1 str2) (check-mapping str2 str1))))
