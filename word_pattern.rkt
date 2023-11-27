#lang racket
(require racket)

(define (all-equal? ls)
  (andmap (λ (a) (equal? a (car ls))) ls))

(define pattern "abba")
(define s "dog cat cat dog")

(define (item-wise-equal? . args)
  (all-equal? (map length args)))


(let ([pats (filter (compose not zero? string-length) (string-split pattern ""))]
      [strs (string-split s)])
  (item-wise-equal? pats strs))

(define (item-wise-equal? . args)
  (and
    (apply equal? (map length args))
    (apply
      andmap
      (map
        (lambda (group) (apply equal? (map length (map remove-duplicates group))))
        (apply map list args)))))

(item-wise-equal? )

(define (word-pattern pattern s)
  (let ([pats (filter (compose not zero? string-length) (string-split pattern ""))]
        [strs (string-split s)])
  (if (and (equal? (length pats) (length strs))
           (apply equal? (map length (map remove-duplicates (list pats strs)))))
      (let ([group (group-by car (map cons pats strs))])
           (andmap (λ (group) (all-equal? (map cdr group))) group))
      #f)))

(module+ test
  (require rackunit)
  (check-equal? (word-pattern "abba" "dog cat cat dog") #t)
  (check-equal? (word-pattern "abba" "dog dog dog dog") #f)
  (check-equal? (word-pattern "aba" "cat cat cat dog") #f)
  (check-equal? (word-pattern "a" "a") #t)
  (check-equal? (word-pattern "abaaa" "dog cat cat dog dog") #f))
