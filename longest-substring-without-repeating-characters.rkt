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


;; try again

;; idea
;; traverse the string w string->list
;; keep a floating hashmap going
;; if count is 1 make a new map and add the char to it
;;    iterate and add 1 to counter and 1 to highest count
;; if count is not 1, check if current char is in hashmap
;;  if so, reset count to 1 and add current char to it
;; at the end of the list, return highest count



(define (length-of-longest-substring s)
  (let iter ([curr-count 1]
             [highest 1]
             [seen (make-hash)]
             [sls (string->list s)])
    (if (null? sls)
        highest
        (match (list curr-count highest)
          [1 (iter (add1 curr-count))]))))

;; make a function
;; take letters until a duplicate is found
(define (seen-before? v hsh)
  (if (hash-has-key? hsh v) #t #f))

(define (take-while-unseen ls [acc '()])
  (let loop ([sb (hash)]
             [vals ls]
             [acc acc])
    (cond [(null? vals) (list acc)]
          [(seen-before? (first vals) sb) (cons acc (take-while-unseen (rest ls) '()))]
          [else
           (loop (hash-set sb (first vals) 1) (rest vals) (append acc (list (first vals))))])))

(define (length-of-longest-substring s)
  (cond [(zero? (string-length s)) 0]
        [(= (string-length s) 1) 1]
        [else (apply max (map length (take-while-unseen (string->list s))))]))

(length-of-longest-substring "abcabcbb")
(length-of-longest-substring "bbbbbb")
(length-of-longest-substring "pwwkew")

(length-of-longest-substring "")
(length-of-longest-substring " ")
(length-of-longest-substring "au")

(take-while-unseen (string->list "au"))
