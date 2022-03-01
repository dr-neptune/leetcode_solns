 #lang racket
(require racket)

;; simple version
;; sort both
;; then check if first of note is a member of magazine
;; if so, rest note and rest member first note for magazine
(define (can-construct ransomNote magazine)
  (let letters ([note (sort (string->list ransomNote) char<?)]
                [mag (sort (string->list magazine) char<?)])
    (if (empty? note)
        #t
        (let ([mem (member (first note) mag)])
          (if mem
              (letters (rest note) (rest mem))
              #f)))))

;; fill up a hash with mag letters counts
;; then for each note letter, check if it is in the mag hash
;; if not, return #f
;; if so, check if its value is >0. If so, decrease by 1. If not, return #f
;; if nums are empty, return true
(define (can-construct ransomNote magazine)
  (let ([counts (make-hash)]
        [mag (string->list magazine)])
    (for-each (λ (v) (hash-update! counts v add1 0)) mag)
    (let note-letters ([note (string->list ransomNote)])
      (cond [(empty? note) #t]
            [(let ([ref (hash-ref counts (first note) #f)]) (and ref (> ref 0)))
             (begin
               (hash-update! counts (first note) sub1)
               (note-letters (rest note)))]
            [else #f]))))
