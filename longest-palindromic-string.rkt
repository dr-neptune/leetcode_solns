#lang racket
(require racket)

(define exstr1 "babad")  ;; bab
(define exstr2 "cbbd")   ;; bb
(define exstr3 "aacabdkacaa") ;; aca


(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))


(define (all-slides ls [k 0] [vals '()])
  (if (= k (length ls))
      vals
      (all-slides ls (add1 k) (append vals (sliding-window ls (add1 k))))))


(define (is-palindrome ls)
  (equal? ls (reverse ls)))


(define (longest-palindrome s)
  (let ([largest '(0 0)])
    (for ([val (in-list (all-slides (string->list s)))])
      (when (is-palindrome val)
        (when (> (length val) (first largest))
          (set! largest (list (length val) val)))))
    (list->string (second largest))))


(longest-palindrome exstr1)
(longest-palindrome exstr2)
(longest-palindrome exstr3)

;; now add dynamic programming
(define-syntax define/memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       (let ([results (make-hash)])
         (lambda (args ...)
           ((lambda vals
              (when (not (hash-has-key? results vals))
                (hash-set! results vals (begin bodies ...)))
              (hash-ref results vals))
            args ...))))]))

(define/memoized (longest-palindrome s)
  (let ([largest '(0 0)])
    (for ([val (in-list (all-slides (string->list s)))])
      (when (is-palindrome val)
        (when (> (length val) (first largest))
          (set! largest (list (length val) val)))))
    (list->string (second largest))))




(define exstr4 "civilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth")


(time (longest-palindrome exstr4))

(define (lp s dict)
  (let ([largest '(0 0)])
    (for ([val (in-list (all-slides (string->list s)))])
      (when (is-palindrome val)
        (when (> (length val) (first largest))
          (set! largest (list (length val) val)))))
    (list->string (second largest))))


;; other solution from prikshet21
