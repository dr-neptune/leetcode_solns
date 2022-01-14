#lang racket
(require racket)

(define (is-palindrome s)
  (define (strip-non-alnum s)
    (filter (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))
            (string->list (string-downcase s))))
  (let* ([fstr-ls (strip-non-alnum s)]
         [fstr (list->string fstr-ls)]
         [fstr-rev (list->string (reverse fstr-ls))])
    (string=? fstr fstr-rev)))

(define exstr "A man, a plan, a canal: Panama")

(is-palindrome exstr)
