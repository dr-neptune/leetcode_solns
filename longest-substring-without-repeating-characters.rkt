#lang racket
(require racket)

(define (count-unique str)
  (let rc ([ls str] [letter-count 0] [hsh (make-hash)])
    (if (or (empty? ls)
            (hash-has-key? hsh (first ls)))
        letter-count
        (rc (rest ls) (add1 letter-count) (begin (hash-update! hsh (first ls) add1 0)
                                                 hsh)))))

(define (traverse-ls ls proc)
  (if (empty? ls)
      '()
      (cons (proc ls) (traverse-ls (rest ls) proc))))

(define (length-of-longest-substring s)
  (let ([substr-lens (traverse-ls (string->list s) count-unique)])
    (if (empty? substr-lens)
        0
        (apply max substr-lens))))

(length-of-longest-substring "bbbbbb")
