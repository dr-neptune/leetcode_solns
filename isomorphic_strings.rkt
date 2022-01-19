#lang racket
(require racket)

;; e -> a
;; g -> d
;; g -> d
;; true

;; f -> b
;; o -> a
;; o -> a , should be to r
;; false

;; p -> t
;; a -> i
;; p -> t
;; e -> l
;; r -> e
;; true

;; idea
;; make a hash table in which the first letter of the first word is the key and the first letter of the second word is the value
;; if key exists but the first letter of the second != value, then #f
;; if key exists and the first letter of the second == value, then continue
;; if empty then true



(define (is-isomorphic s t)
  (let strings ([together (map list (string->list s) (string->list t))]
                [dict (make-hash)])
    (match together
      [(list (list f s) rest) (cond [(and (hash-has-key? dict f)
                                          (= (hash-ref dict f) s))
                                     (strings (rest together) dict)]
                                    [(and (hash-has-key? dict f)
                                          (not (= (hash-ref dict f) s)))
                                     #f]
                                    [else
                                     (begin
                                       (hash-update! dict f s)
                                       (strings fstr sstr (rest together) dict))])]
      [(list) #t])))



(define (str-dict-compare fstr sstr dict)
  (cond [(and (hash-has-key? dict fstr)
              (equal? (hash-ref dict fstr) sstr)) #t]
        [(and (hash-has-key? dict fstr)
              (not (equal? (hash-ref dict fstr) sstr))) #f]
        [else
         (begin (hash-update! dict fstr sstr)
                #t)]))


(define dict (make-hash))

(str-dict-compare #\p #\t dict)


(is-isomorphic exstr1 exstr2)


(define exstr1 "paper")
(define exstr2 "title")

(first (map list (string->list exstr1)
            (string->list exstr2)))

(define example-duo
  (map list (string->list exstr1)
            (string->list exstr2)))

(rest (rest (rest (rest (rest example-duo)))))
