#lang racket
(require racket (rename-in racket [partition base/partition]))

#|

idea

make an is-palindrome? fn

then find all substrings of size 1 -> n
and keep only the palindromes
use streams

|#


(define (is-palindrome-ls? ls)
  (equal? ls (reverse ls)))

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(let ([s "aab"])
  (let ([strls (string->list s)])
    (let loop ([partition-size 1])
      (cond [(equal? (length strls) partition-size) '()]
            [else
             (cons (sliding-window strls partition-size)
                   (loop (add1 partition-size)))]))))

(sliding-window '(1 2 3 4) 2)


(let ([s "aab"])
  (let ([strls (string->list s)])
    (for/fold ([palindromes '()])
              ([partition-size (in-range 1 (length strls))])
      (let ([windows (sliding-window strls partition-size)])
        (values (cons (map list->string (filter is-palindrome-ls? windows)) palindromes))))))

#|

what they want is more subtle
where can I split a string such that lhs and rhs are both palindromes

idea

iterate through the strls

|#

(define (palindrome-split ls)
  (for/fold ([results '()])
            ([idx (in-range (length ls))])
    (match-let ([(list lhs rhs)
                 (call-with-values (λ () (split-at ls idx)) list)])
      (if (and (is-palindrome-ls? lhs)
               (is-palindrome-ls? rhs))
          (values (cons (list lhs rhs) results))
          '()))))


(palindrome-split (string->list "aab"))

#|

maybe something like split-into n chunk s

|#

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))




(split-into '(#\a #\a #\b) 3)


(let ([s "aab"])
  (let ([strls (string->list s)])
    (for/fold ([palindromes '()]
               #:result (map
                         (λ (lol) (map list->string lol))
                         (filter (λ (ls)
                                  (andmap is-palindrome-ls? ls)) palindromes)))
              ([partition-size (in-inclusive-range 1 (length strls))])
      (let ([windows (split-into strls partition-size)])
        (values (cons windows palindromes))))))

(define (is-palindrome-ls? ls)
  (equal? ls (reverse ls)))

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (partition s)
  (let ([strls (string->list s)])
    (for/fold ([palindromes '()]
               #:result (map
                         (λ (lol) (map list->string lol))
                         (filter (λ (ls)
                                   (andmap is-palindrome-ls? ls)) palindromes)))
              ([partition-size (in-inclusive-range 1 (length strls))])
      (let ([windows (split-into strls partition-size)])
        (displayln windows)
        (values (cons windows palindromes))))))


(partition "cdd")

#|

idea
backtracking

split-into update
move over a list ele by ele
if lhs and rhs are palindromes, add them to a list
finish out the row so we get symmetries, like '((a) (b b)) and '((a b) (b))


|#

(let ([exls '(1 2 3 4 5 6)])
  (let split-into ([ls exls]
                   [subls-size 0]))
  ;; to be continued
  )

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))
