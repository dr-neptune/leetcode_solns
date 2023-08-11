#lang racket
(require racket)

;; idea
;; string->list
;; take a set and convert it to a str
;; see if the entire list

;; maybe something fancy with the comparator in sort?
;; string to list to set
;; for len set, partition
;; see if every element of the resulting partitioned set is equal to set
(define (partition-n ls n)
  (match (length ls)
    [0 '()]
    [(? (λ (v) (< v n))) (list ls)]
    [_ (cons (take ls n)
             (partition-n (drop ls n) n))]))

;; dumb way
;; take 1 letter
;; iterate through list and check if each letter is that letter
;; if eol, return #t
;; take 2 letters
;;
;; take n letters
;; return #f
(define (partition-n ls n)
  (match (length ls)
    [0 '()]
    [(? (λ (v) (< v n))) (list ls)]
    [_ (cons (take ls n)
             (partition-n (drop ls n) n))]))

(define (all-equal? ls)
  (andmap (λ (a) (equal? a (car ls))) ls))

(define (repeated-substring-pattern s)
  (define string-ls (string->list s))
  (define (iter num-letters)
    (if (= num-letters (add1 (quotient (length string-ls) 2)))
        #f
        (let ([parts (map list->string (partition-n string-ls num-letters))])
          (if (all-equal? parts)
              #t
              (iter (add1 num-letters))))))
    (iter 1))

(repeated-substring-pattern "ababab")
(repeated-substring-pattern "abababa")
(repeated-substring-pattern "a")
(repeated-substring-pattern "")

;; if more than half, obv no repeats
;; if a single digit, no repeats
;; get partitions and see if all equal
;;    if yes, then return true
;;    if no, up the iterator

(all-equal? (map list->string (partition-n (string->list "abababa") 2)))


;; new idea
;; if len == 1 or 0, then #f
;; else string->list sort
;; check if all the letters have the same amount of instances
(define (all-equal? ls [val (car ls)])
  (andmap (λ (a) (equal? a val)) ls))

(all-equal? '(1 1 1 1))
(all-equal? '(2 2) 2)

(define (count-elements lst)
  (map (lambda (pair) (cons (car pair) (length (cdr pair))))
       (group-by (lambda (x) x) lst)))

(define (repeated-substring-pattern s)
  (let* ([string-ls (string->list s)]
         [counts (count-elements string-ls)])
    (if (or (= 0 (string-length s)) (= 1 (string-length s)))
        #f
        (all-equal? (map cdr counts)))))


(combinations (string->list "abab"))

string

(string-split "ab ab ab" #:repeat? #t)

;; idea
;;

(define (all-equal? ls [val (car ls)])
  (andmap (λ (a) (equal? a val)) ls))

(define (repeated-substring-pattern s)
  (if (< (string-length s) 2)
      #f
      (let iter ([i 0])
        (if (> i (quotient (string-length s) 2))
            #f
            (let* ([sub (substring s 0 i)])
              (begin
                (displayln (format "iteration: ~a substring: ~a" i sub))
                (if (all-equal? (string-split s sub) "")
                    #t
                    (iter (add1 i)))))))))

(repeated-substring-pattern "ababab")
(repeated-substring-pattern "abababa")
(repeated-substring-pattern "a")
(repeated-substring-pattern "")


(define (repeated-substring-pattern s)
  (let* ([double (string-append s s)]
         [t (substring double 1 (sub1 (string-length double)))])
    (string-contains? t s)))

;; this is lame
