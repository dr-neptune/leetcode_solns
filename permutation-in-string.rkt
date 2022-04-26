#lang racket
(require racket)

;; idea
;; naive
;; get all permutations of s1
;; check if each one is in s2 until #t, else #f
(define (check-inclusion s1 s2)
  (let ([s1ls (string->list s1)])
    (let rc ([perms (stream-map list->string ((compose sequence->stream in-permutations) s1ls))])
      (cond [(stream-empty? perms) #f]
            [(string-contains? s2 (stream-first perms)) #t]
            [else (rc (stream-rest perms))]))))

(check-inclusion "ab" "eidbaooo")

(check-inclusion
 "algorithm"
 "altruistic")

(check-inclusion
 "abcdxabcde"
 "abcdeabcdx")

;; even with streams, it is too slow!

;; new idea
;; for a permutation to exist in s2, s2 must have all the letters of s1 in some order
;; next to each other. Thus, we can slide across s2 with a window of (length s1) checking
;; to see if every letter in s1 exists in s2

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(sliding-window '(1 2 3 4 5) 2)

(hash-count)

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(hash-table-counter (string->list "hello"))


(first (sliding-window-counter (string->list "helloworld") 2))
(second (sliding-window-counter (string->list "helloworld") 2))

(last (map hash-table-counter (sliding-window (string->list "helleh") 3)))

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (check-inclusion s1 s2)
  (if (> (string-length s1) (string-length s2)) #f
      (let ([s1hash (hash-table-counter (string->list s1))]
            [s2hashes (map hash-table-counter
                           (sliding-window (string->list s2) (string-length s1)))])
        (let rc ([s2h s2hashes])
          (cond [(empty? s2h) #f]
                [(equal? (first s2h) s1hash) #t]
                [else (rc (rest s2h))])))))

;; now make it do a single pass
(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (sliding-window ls n cmp-hash [vals '()])
  (cond [(= n (length ls))
         (let ([ht (hash-table-counter ls)])
           (if (equal? cmp-hash ht)
               #t
               #f))]
        [else (let ([ht (hash-table-counter (take ls n))])
                (if (equal? cmp-hash ht)
                    #t
                    (sliding-window (rest ls) n cmp-hash (cons ht vals))))]))

(define (check-inclusion s1 s2)
  (if (> (string-length s1) (string-length s2))
      #f
      (let ([sls1 (string->list s1)]
            [sls2 (string->list s2)])
        (sliding-window sls2 (string-length s1) (hash-table-counter sls1)))))


(define exstr1 "abcdxabcde")
(define exstr2 "abcdeabcdx")

(check-inclusion
 "abcdxabcde"
 "abcdeabcdx")

(check-inclusion
 "ab"
 "eidboaoo")

;; drat, still too slow!
;; new idea (taken from solutions)
;; when incrementing the sliding window, we only need to remove the first letter and add the last letter to the hashmap. Thus we can have a snapshot to compare to our s1 hsh counter

;; initial map
(define hsh (hash-table-counter (string->list (substring "eidboaoo" 0 2))))

(define (sliding-window-counter ls n)
  (let ([init-hash (hash-table-counter (take ls n))])
    (let rc ([hsh init-hash])
      (begin
        (println (format "hsh: ~a ls: ~a" hsh ls))
        (if (= n (length ls))
            ()
            (equal? cmp-hash
                    (hash-update hsh )))))))


;; if eq #t
;; else recurse with sub1 first add1 last
(define (sliding-hc ls n hc cmp-hash)
  (let rc ([ls ls]
           [subls (take ls n)])
    (begin
      (println (format "subls ~a hsh ~a" subls hsh))
      (if (equal? cmp-hash hc)
        #t
        (rc (rest ls) n (begin
                          (hash-update! hc (first subls) sub1)
                          (hash-update! hc (last subls) add1 0)
                          hc) cmp-hash)))))

(sliding-hc (string->list "helloworld") 2 (hash-table-counter (string->list "he"))
            (hash-table-counter (string->list "ow")))



;; when we take the second window we subtract the first and add the last
(substring "eidbaooo" 1 3)

(hash-update! hsh #\e sub1)
(hash-update! hsh #\d add1 0)

;; make it simpler
(define init-hash (hash-table-counter (string->list "ei")))

;; check if each letter in s1 is 0
(= (string-length "ab") (apply + (map (λ (k) (hash-ref init-hash k 0)) (string->list "ab"))))

;; now we take the next sliding window id
(hash-update! init-hash #\e sub1)
(hash-update! init-hash #\d add1 0)

;; check if each letter in s1 is 0
(= (string-length "ab") (apply + (map (λ (k) (hash-ref init-hash k 0)) (string->list "ab"))))

(define (sliding-window-hc ls n s1)
  "ls, s1 : strls"
  (define (check-comp s1 init-hash)
    (= (length s1)
       (apply + (map (λ (k) (if (hash-ref init-hash k #f) 1 0)) s1))))
  ;; update last
  (let rc ([init-hash (hash-table-counter (take ls n))]
           [ls ls])
    (cond [(check-comp s1 init-hash) #t]
          [else
           (rc (begin
                 (hash-update! init-hash (first ls))))])))

  ;; recurse update first)

;; taking the solution
(map (λ (v) 0) (range 26))
