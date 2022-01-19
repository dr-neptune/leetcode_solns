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
         (begin (hash-set! dict fstr sstr)
                #t)]))



(define dict (make-hash))

(str-dict-compare #\b #\b dict)

(str-dict-compare #\a #\a dict)

(str-dict-compare #\d #\b dict)

(str-dict-compare #\c #\a dict)

(str-dict-compare #\p #\t dict)

(str-dict-compare #\a #\i dict)

(str-dict-compare #\p #\t dict)

(str-dict-compare #\e #\l dict)

(str-dict-compare #\r #\e dict)

(str-dict-compare #\p #\s dict)


(define (str-dict-compare fstr sstr dict)
  (cond [(and (hash-has-key? dict sstr)
              (equal? (hash-ref dict sstr) fstr)) #t]
        [(and (hash-has-key? dict sstr)
              (not (equal? (hash-ref dict sstr) fstr))) #f]
        [else
         (begin (hash-set! dict sstr fstr)
                #t)]))

(define (is-isomorphic s t)
  (let through ([fstr (string->list s)]
                [sstr (string->list t)]
                [dict (make-hash)])
    (cond [(empty? fstr) #t]
          [(false? (str-dict-compare (first fstr) (first sstr) dict)) #f]
          [else (through (rest fstr) (rest sstr) dict)])))


(is-isomorphic exstr1 exstr2)
(is-isomorphic "foo" "bar")
(is-isomorphic "egg" "add")
(is-isomorphic "" "")
(is-isomorphic "badc" "baba")



(define exstr1 "paper")
(define exstr2 "title")

;; idea
;; add all second string items as keys
;; add maps from first
;; if there is a collision
