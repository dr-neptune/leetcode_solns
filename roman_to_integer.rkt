#lang racket
(require racket)

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


(define (char->symbol chrs)
  "Converts character(s) or lists of character(s) to a symbol"
  (match chrs
    [(or (list a b) (cons a b)) (string->symbol (string a b))]
    [(or a (list a)) (string->symbol (string a))]))


(define (roman-to-int str)
  (define (roman->int slist [val 0])
    "traverse the string list, checking against the *rules* and add up the returned values"
    (let ([two-char (if (>= (length slist) 2)
                        (hash-ref *rules* (char->symbol (take slist 2)) #f)
                        #f)]
          [one-char (if (>= (length slist) 1)
                        (hash-ref *rules* (char->symbol (first slist)))
                        #f)])
      (cond [(empty? slist) val]
            ;; check for 2 rules (IV, CM, etc). If found, move ahead 2
            [two-char
             (roman->int (drop slist 2) (+ val two-char))]
            ;; otherwise return a single rule
            [one-char
             (roman->int (rest slist) (+ val one-char))])))
  (roman->int (string->list str)))


(module+ test
  (require rackunit)
  (check-equal? (roman-to-int "I") 1)
  (check-equal? (roman-to-int "III") 3)
  (check-equal? (roman-to-int "LVIII") 58)
  (check-equal? (roman-to-int "MCMXCIV") 1994))
