#lang racket
(require racket)


(let ([sentence1 '("great" "acting" "skills")]
      [sentence2 '("fine" "drama" "talent")]
      [similarPairs '(("great" "fine")
                      ("drama" "acting")
                      ("skills" "talent"))])
  (let ([hsh (for/hash ([pair (append similarPairs (map reverse similarPairs))])
               (values (first pair) (second pair)))])
    (cond [(not (equal? (length sentence1) (length sentence2))) #f]
          [else
           (for/and ([s1 sentence1]
                     [s2 sentence2])
             (equal? (hash-ref hsh s1)
                     (hash-ref hsh s2)))])))


(let ([sentence1 '("great")]
      [sentence2 '("great")]
      [similarPairs '()])
  (let ([hsh (for/hash ([pair (append similarPairs (map reverse similarPairs))])
               (values (first pair) (second pair)))])
    (cond [(equal? (length sentence1) (length sentence2)) #t]
          [else
           (for/and ([s1 sentence1]
                     [s2 sentence2])
             (equal? (hash-ref hsh s1)
                     (hash-ref hsh s2)))])))


(let ([sentence1 '("great")]
      [sentence2 '("doubleplus" "good")]
      [similarPairs '(("great" "doubleplus"))])
  (let ([hsh (for/hash ([pair (append similarPairs (map reverse similarPairs))])
               (values (first pair) (second pair)))])
    (cond [(equal? (length sentence1) (length sentence2)) #t]
          [else
           (for/and ([s1 sentence1]
                     [s2 sentence2])
             (equal? (hash-ref hsh s1)
                     (hash-ref hsh s2)))])))


(define (are-sentences-similar sentence1 sentence2 similarPairs)
  (let ([hsh (for/hash ([pair (append similarPairs (map reverse similarPairs))])
               (values (first pair) (second pair)))])
    (cond [(not (equal? (length sentence1) (length sentence2))) #f]
          [else
           (for/and ([s1 sentence1]
                     [s2 sentence2])
             (equal? (hash-ref hsh s1 #f)
                     (hash-ref hsh s2 #f)))])))

(let ([sentence1 '("great" "acting" "skills")]
      [sentence2 '("fine" "painting" "talent")]
      [similarPairs '(("great" "fine")
                      ("drama" "acting")
                      ("skills" "talent"))])
  (let ([hsh (for/hash ([pair (append similarPairs (map reverse similarPairs))])
               (values (first pair) (second pair)))])
    (cond [(not (equal? (length sentence1) (length sentence2))) #f]
          [else
           (for/and ([s1 sentence1]
                     [s2 sentence2])
             (displayln (format "s1: ~a\ts2: ~a" s1 s2))
             (equal? (hash-ref hsh s1)
                     (hash-ref hsh s2)))])))
