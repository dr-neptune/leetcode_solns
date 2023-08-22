#lang racket
(require racket)

(define (partition-n ls n)
  (match (length ls)
    [0 '()]
    [(? (λ (v) (< v n))) (list ls)]
    [_ (cons (take ls n)
             (partition-n (drop ls n) n))]))

(define (all-equal? ls [val (car ls)])
  (andmap (λ (a) (equal? a val)) ls))

(define (gcd-of-strings str1 str2)
  (let* ([divisor (apply gcd (map string-length (list str1 str2)))]
         [sls1 (string->list str1)]
         [sls2 (string->list str2)]
         [ssub (take sls1 divisor)])
    ;; check if strings are both made up of only that subset
    (match (and (all-equal? (partition-n sls1 (length ssub)) ssub)
                (all-equal? (partition-n sls2 (length ssub)) ssub))
      [#t (list->string ssub)]
      [_ ""])))
