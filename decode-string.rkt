#lang racket
(require racket)
regexp
#|

idea
can we use parser combinators without a library?

|#

(define (take-numbers strls)
  (takef strls char-numeric?))

(let ([s "3[a]2[bc]"])
  (let ([strls (string->list s)])
    (take-numbers strls)))
