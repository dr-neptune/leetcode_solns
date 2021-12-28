#lang racket
(require racket)

;; way overthinking it

;; new idea
;; loop over the string list
;; if cur and next in dict, add to sum and jump ahead 2
;; otherwise if cur in dict, add to sum and jump ahead 1

;; idea
;; make a hash map with the basics and the subtraction rules
;; parse the string, checking the value and the value ahead
;; if none of the subtraction rules hold for the next 2 digits,
;; take the current value and add it to the total. Otherwise
;; take the subtraction value and add it to the total and move
;; ahead 2 values

;; define immutable hash tables
(define *rules*
  (hash
   'I 1
   'V 5
   'X 10
   'L 50
   'C 100
   'D 500
   'M 1000
   'IV 4
   'IX 9
   'XL 40
   'XC 90
   'CD 400
   'CM 900))

(define (pairwise-slide ls)
  (cond [(or (= 1 (length ls))
             (= 2 (length ls))) (list ls)]
        [(cons
          (cons (first ls)
                (first (rest ls)))
          (pairwise-slide (rest ls)))]))

;; if the concatenation is in pair-rules, then return the number
;; otherwise look up the first value in single-rules and add that
(define example (pairwise-slide (string->list "MCMXCIV")))


(define (char->symbol pr)
  (match pr
    [(cons a b) (string->symbol (string a b))]
    [a (string->symbol (string a))]))


(define (check-dict tuple)
  (let* ([sym (char->symbol tuple)]
         [val (hash-ref *rules* sym #f)])
  (if val
      (list 1 sym val)
      (let* ([sym (char->symbol (car tuple))]
             [val (hash-ref *rules* sym #f)])
        (list 0 sym val)))))

;; (pairwise-slide (string->list "III"))
;; (pairwise-slide (string->list "I"))
;; (roman-to-int "IV")


(define (check-pairs pairs)
  (let* ([cur-val (first pairs)]
         [dict-result (check-dict cur-val)])
    (cond [(= 1 (length pairs)) (list dict-result)]
          [(= 1 (first dict-result))
           (cons dict-result
                 (check-pairs (cddr pairs)))]
          [(cons dict-result
                 (check-pairs (cdr pairs)))])))


(define (roman-to-int str)
  (let ([pairs (pairwise-slide (string->list str))])
    (foldl + 0 (map last (check-pairs pairs)))))


(module+ test
  (require rackunit)
  (check-equal? (roman-to-int "I") 1)
  (check-equal? (roman-to-int "III") 3)
  (check-equal? (roman-to-int "LVIII") 58)
  (check-equal? (roman-to-int "MCMXCIV") 1994))
