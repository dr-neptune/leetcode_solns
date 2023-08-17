#lang racket
(require racket)

;; idea
;; iterate over both strings until they don't match
(define (efirst ls)
  (if (null? ls)
      '()
      (first ls)))

(define (gcd-of-strings str1 str2)
  (define rev-str (compose list->string reverse))
  (let iter ([s1 (string->list str1)]
             [s2 (string->list str2)]
             [acc '()])
    (displayln (format "~a ~a ~a" s1 s2 acc))
    (match (list (efirst s1) (efirst s2))
      [(list '() '()) (rev-str acc)]
      [(list a a) (iter (rest s1) (rest s2) (cons a acc))]
      [_ (rev-str acc)])))

(gcd-of-strings "ABCABC" "ABC")
(gcd-of-strings "ABABAB" "ABAB")


(define (gcd-of-strings str1 str2)
  (let ([s1 (string->list str1)]
        [s2 (string->list str2)])
    (let-values ([(common-prefix l r) (split-common-prefix s1 s2)])
      (if (null? common-prefix)
          ""
          (list->string (argmax length (list l r)))))))

;; need to rethink this approach

(split-common-prefix (string->list "ABCDEF") (string->list "ABC"))
