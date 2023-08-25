#lang racket
(require racket)

;; idea
;; traverse once and collect all the vowels
;; traverse a second time and replace values (take-right)
(define (vowel? c)
    (match c
      [(or #\a #\e #\i #\o #\u
           #\A #\E #\I #\O #\U) #t]
      [_ #f]))

(define (reverse-vowels s)
  (let* ([chars (string->list s)]
         [vowels (for/list ([c chars]
                            #:when (vowel? c))
                   c)])
    (let iter ([str chars]
               [vs (reverse vowels)]
               [acc '()])
      (cond [(null? str) (list->string (reverse acc))]
            [(vowel? (first str)) (iter (rest str)
                                        (rest vs)
                                        (cons (first vs) acc))]
            [else
             (iter (rest str) vs (cons (first str) acc))]))))


(define (reverse-vowels s)
  (let* ([chars (string->list s)]
         [vowels (for/list ([c chars]
                            #:when (vowel? c))
                   c)])
    (for/list ([s chars]
               [k vowels]
               #:when (vowel? s)
               [k vowels])
      ???)))


(define chars (string->list "leetcode"))
(define vowels (list #\e #\e #\o #\e))

result: "leotcede"

(for/fold ([s chars]
           [v vowels]))



(define (reverse-vowels str vowels)
  (let* ([chars (string->list str)]
         [reversed-vowels (reverse vowels)])
    (for/fold ([new-str '()] [vowels reversed-vowels])
              ([char chars])
      (cons (if (member char '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
                  (begin (set! vowels (cdr vowels))
                         (if vowels (car vowels) char))
                  char)
            new-str))))

(reverse-vowels "leetcode" vowels)
(define str "leetcode")
(define vowels (list #\e #\e #\o #\e))
(list->string (reverse (reverse-vowels str vowels)))

(for/foldr ([acc '()])
            ([s chars])
            ;; ([v vowels])
           (cons v acc))

(define (in-printing seq)
  (sequence-map (lambda (v) (println v) v) seq))

(for/foldr ([acc '()])
           ([v (in-printing (in-range 1 4))])
  (println v)
  (cons v acc))

(for/foldr ([result '()])
           ([s chars]
            [v vowels])
  (cons (if (vowel? s) v s) result))

(define str "leetcode")
(define chars (string->list str))
(define vowels (list #\e #\e #\o #\e))

(for/foldr ([result '()])
           ([s chars]
            [v vowels])
  (cons (if (vowel? s) v s) result))

(reverse-vowels "leetcode")
(reverse-vowels "hello")
(reverse-vowels "b")


(for/foldr ([acc '()])
           ([]))


(define (vowel? c)
  (match c
    [(or #\a #\e #\i #\o #\u
         #\A #\E #\I #\O #\U) #t]
    [_ #f]))

(define (reverse-vowels* s)
  (define chars (string->list s))
  (for/foldr ([acc '()]
              [vowels (for/list ([c chars] #:when (vowel? c)) c)]
              #:result (list->string acc))
             ([ch chars])
    (if (vowel? ch)
        (values (cons (first vowels) acc)
                (rest vowels))
        (values (cons ch acc)
                vowels))))


(for/foldr ([acc '()]
            [vowels (for/list ([c chars] #:when (vowel? c)) c)]
            #:result (list->string acc))
           ([ch (in-list chars)])
  (if (vowel? ch)
      (values (cons (first vowels) acc)
              (rest vowels))
      (values (cons ch acc)
              vowels)))

;; why use in-list?
;; why is vowels in the first section with the accumulator?

(reverse-vowels* "axexixoxu")


(for/foldr ([acc '()]
            [v (for/list ([c chars] #:when (vowel? c)) c)]
            #:result (list->string acc))
           ([ch chars])
  (if (vowel? ch)
      (values (cons (first v) acc) (rest v))
      (values (cons ch acc) v)))
