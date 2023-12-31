#lang racket
(require racket)

#|

idea

turn both inputs into lists of chars

including duplicates: we need to keep track of each character in some kind of stack

take-while t has values in it. when one is found, pop it off the stack

if all values are found, set that as the answer := min(answer, num values)


|#


(let ([s "ADOBECODEBANC"]
      [t "ABC"])
  t)


#|

(a) (b c) (a)
(a d) (b c) (a)
(a d o) (b c) (a)
(a d o b) (c) (a b)
(a d o b e) (c) (a b)
(a d o b e c) () (a b c)  -> found all 3, answer = 6
now we want to lighten the lhs until we find a value in our third stack. then push third stack to first stack
(d o b e c) (a) (b c)
(d o b e c o) (a) (b c)
(d o b e c o d) (a) (b c)
(d o b e c o d e) (a) (b c)
(d o b e c o d e b) (a) (b c)
(d o b e c o d e b a) () (b c a) -> found all 3, answer = min(6, 10) = 6
lighten lhs
(o d e b a) (c) (b a)
...
(o d e b a n c) () (b a c) -> found all 3, answer = min(6, 7) = 6
lighten lhs
(b a n c) () (b a c) -> found all 3, answer = min(6, 7) = 6


;; adjust the lightening function
(a) (b c) (a)
(a d) (b c) (a)
(a d o) (b c) (a)
(a d o b) (c) (a b)
(a d o b e) (c) (a b)
(a d o b e c) () (a b c)  -> found all 3, strip and answer = 6
;; pop first back into ls 1 and move lhs to next instance
(d o b e c) (a) (b c)
(b e c) (a) (b c)
;; now add values until we find something in ls 1
(b e c o d e b a) -> all 3 found, answer = min(6, 8) = 6
;; pop first back into ls 1 and move lhs to next instance
(c o d e b a)

|#

#|

2 pointers

start 2 pointers at 0 (lhs), loc of last val in t (rhs)

check if all characters are in the window
if so, answer = min(answer, rhs - lhs)

0 12 (a d o b e c o d e b a n c)

then recurse on lhs and rhs to next val in t

lhs
3 12 (a d o | b e c o d e b a n c)

ans = min(12, 9) = 9

- lhs
5 12 (a d o | b e | c o d e b a n c)

ans = min(9, 8) = 8

-- lhs
9 12 (a d o | b e | c o d e | b a n c)
ans = min(8, 12 - 9 + 1 = 4) = 4

-- rhs
5 10 (a d o | b e | c o d e b a | n c)
ans = min(8, 10 - 5 + 1 = 6) = 6

- rhs
3 9 (a d o | b e c o d e b | a n c)
-> dnc all values, stop

rhs
0 9 (a d o b e c o d e b | a n c)

ans = min(ans, 9) = 9

- lhs
0 9 (a d o | b e c o d e b | a n c)

|#

;; values in ls



;; find next value (add direction)
(let ([vec (list->vector (string->list "ADOBECODEBANC"))])
  (apply min (map (λ (e) (vector-member e vec)) '(#\C #\B))))

(let ([vec (list->vector (string->list "ADOBECODEBANC"))])
  (apply max (map (λ (e) (vector-member-right e vec)) '(#\B))))

(let ([vec (list->vector (string->list "ADOBECODEBANC"))])
  (for/vector ([idx (in-range 3 9)])
    (vector-ref vec idx)))

(define (vector-member-right ele vec)
  (let loop ([idx (sub1 (vector-length vec))])
    (cond [(> 0 idx) #f]
          [(equal? ele (vector-ref vec idx)) idx]
          [else (loop (sub1 idx))])))

(define (get-next-l vec vals)
  (apply min (map (λ (e) (vector-member e vec)) vals)))

(define (get-next-r vec vals)
  (apply max (map (λ (e) (vector-member-right e vec)) vals)))

(define (check-members vec eles)
  (andmap (λ (e) ((compose not false?) (vector-member e vec))) eles))

(define (vec-between vec lhs rhs)
  (for/vector ([idx (in-range lhs rhs)]) (vector-ref vec idx)))

(let ([vec (list->vector (string->list "ADOBECODEBANC"))])
  (get-next-l (vec-between vec 4 9) '(#\A #\B #\C)))

(let ([vec (list->vector (string->list "ADOBECODEBANC"))])
  (get-next-r vec '(#\A #\B)))


(let* ([s (list->vector (string->list "ADOBECODEBANC"))]
       [t (string->list "ABC")]
       [answer 1000])
  (let loop ([lhs 0] [rhs (vector-length s)])
    (let ([curr-vec (vec-between s lhs rhs)])
      (displayln (format "lhs: ~a\trhs: ~a\tcurr-vec: ~a\tanswer: ~a" lhs rhs curr-vec answer))
      (if (check-members curr-vec t)
          (begin
            (set! answer (min answer (- rhs lhs)))
            (loop (get-next-l (vector-drop curr-vec 1) t) rhs)
            (loop lhs (get-next-r (vector-drop-right curr-vec 1) t)))
          #f))))


#|

editorial

1. start with lhs and rhs pointers, both pointing to 0
2. use rhs to expand the window until the window contains all characters of t
3. once we have a window with all chars, move lhs forward. If the window still has
   all the characters keep updating the minimum size
4. if the window doesn't have all chars, repeat step 2 onwards
|#

(define (all-members ls eles)
  (andmap (λ (e) ((compose not false?) (member e ls))) eles))

(define (all-members ls eles)
  (cond [(empty? eles) #t]
        [(member (first eles) ls)
         (all-members (remove (first eles) ls) (rest eles))]
        [else #f]))

(define (list-between ls lhs rhs)
  (for/list ([idx (in-range lhs rhs)])
    (list-ref ls idx)))

(list-between '(1 2 3 4 5 6 7 8 9 10) 2 5)

(all-members '(1 2 3 4 5 6 7 8 9 10) '(2 3 8 8))


(let* ([s (string->list "ADOBECODEBANC")]
       [t (string->list "ABC")]
       [answer 1000])
  (let loop ([lhs 0] [rhs 0])
    (displayln (format "~a ~a ~a" lhs rhs answer))
    (cond [(> rhs (length s)) answer]
          [(all-members (list-between s lhs rhs) t)
           (begin
             (set! answer (min answer (- rhs lhs)))
             (loop (add1 lhs) rhs))]
          [else
           (loop lhs (add1 rhs))])))


(let* ([s (string->list "adobecodebanc")]
       [t (string->list "abc")]
       [default-val (make-list 10000 1)]
       [answer default-val])
  (let loop ([lhs 0] [rhs 0])
    (displayln (format "~a ~a" lhs rhs))
    (cond [(> rhs (length s)) (if (equal? answer default-val)
                                  ""
                                  (list->string answer))]
          [(all-members (list-between s lhs rhs) t)
           (begin
             (set! answer (if (> (length answer) (- rhs lhs))
                              (list-between s lhs rhs)
                              answer))
             (loop (add1 lhs) rhs))]
          [else
           (loop lhs (add1 rhs))])))


(define (all-members ls eles)
  (cond [(empty? eles) #t]
        [(member (first eles) ls)
         (all-members (remove (first eles) ls) (rest eles))]
        [else #f]))

(define (list-between ls lhs rhs)
  (for/list ([idx (in-range lhs rhs)])
    (list-ref ls idx)))

(define (min-window s t)
  (let* ([s (string->list s)]
         [t (string->list t)]
         [default-val (make-list 10000 1)]
         [answer default-val])
    (let loop ([lhs 0] [rhs 0])
      (cond [(> rhs (length s)) (if (equal? answer default-val)
                                    ""
                                    (list->string answer))]
            [(all-members (list-between s lhs rhs) t)
             (begin
               (set! answer (if (> (length answer) (- rhs lhs))
                                (list-between s lhs rhs)
                                answer))
               (loop (add1 lhs) rhs))]
            [else
             (loop lhs (add1 rhs))]))))


(min-window "adobecodebanc" "abc")
(min-window "a" "aa")
(min-window "a" "a")


;; time limit exceeded
;; rewrite for vectors
(define (all-members vec eles)
  (equal? (length eles)
          (vector-length
           (vector-filter identity
                          (vector-map (λ (v) ((compose not false? member) v eles)) vec)))))


#|

idea
brute force
loop through vector
if an element is found, drop that element from the list

|#

(define (all-members vec eles)
  (let loop ([elements eles] [idx 0])
    (cond [(empty? elements) #t]
          [(> idx (sub1 (vector-length vec))) #f]
          [(member (vector-ref vec idx) elements)
           (loop (remove (vector-ref vec idx) elements) (add1 idx))]
          [else (loop elements (add1 idx))])))

;; can I make all-members faster?

(define (vec-between vec lhs rhs)
  (for/vector ([idx (in-range lhs rhs)])
    (vector-ref vec idx)))

(define (min-window s t)
  (let* ([s (list->vector (string->list s))]
         [t (string->list t)]
         [default-val (make-vector 10000 1)]
         [answer default-val])
    (let loop ([lhs 0] [rhs 0])
      (cond [(> rhs (vector-length s)) (if (equal? answer default-val)
                                           ""
                                           ((compose list->string vector->list) answer))]
            [(all-members (vec-between s lhs rhs) t)
             (begin
               (set! answer (if (>= (vector-length answer) (- rhs lhs))
                                (vec-between s lhs rhs)
                                answer))
               (loop (add1 lhs) rhs))]
            [else (loop lhs (add1 rhs))]))))

(min-window "adobecodebanc" "abc")
(min-window "a" "aa")
(min-window "a" "a")
(min-window "bba" "ab")
(min-window "acbbaca" "aba")

(define (hash-table-counter vec)
  (let ([counts (make-hash)])
    (for ([v vec])
      (hash-update! counts v add1 0))
    counts))

(define (all-members vec eles)
  (let ([counts (hash-table-counter vec)])
    (for/and ([ele eles])
      (let ([amount (hash-ref counts ele 0)])
        (if (zero? amount)
            #f
            (begin
              (hash-update! counts ele sub1)
              #t))))))

(define (vec-between vec lhs rhs)
  (for/vector ([idx (in-range lhs rhs)])
    (vector-ref vec idx)))

(define (min-window s t)
  (let* ([s (list->vector (string->list s))]
         [t (string->list t)]
         [default-val (make-vector 10000 1)]
         [answer default-val])
    (let loop ([lhs 0] [rhs 0])
      (cond [(> rhs (vector-length s)) (if (equal? answer default-val)
                                           ""
                                           ((compose list->string vector->list) answer))]
            [(all-members (vec-between s lhs rhs) t)
             (begin
               (set! answer (if (>= (vector-length answer) (- rhs lhs))
                                (vec-between s lhs rhs)
                                answer))
               (loop (add1 lhs) rhs))]
            [else (loop lhs (add1 rhs))]))))
